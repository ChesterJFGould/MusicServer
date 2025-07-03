module QStore
( DB
, Tx
, Query
, Var
, Result
, emptyDB
, transact
, query
, parseQuery
, parseTx
) where

import Data.Int
import Data.List
import Debug.Trace
import Ewe
import Sexpr

data Id = Id Int64
  deriving (Eq, Ord, Show)

incId :: Id -> Id
incId (Id x) = Id (x + 1)

data Symbol = Symbol String
  deriving (Eq, Ord, Show)

data Value
  = VId Id
  | VString String
  | VInt Int64
  | VSymbol Symbol
  deriving (Eq, Ord, Show)

data Q = Q { id :: Id, attr :: Symbol, val :: Value, tx :: Id, truth :: Bool }
  deriving (Eq, Show)

instance Ord Q where
  q <= q' = (q.tx, q.id, q.attr, q.val, q.truth) <= (q'.tx, q'.id, q'.attr, q'.val, q'.truth)

data DB = DB { nextId :: Id, qs :: [Q] }
  deriving Show

emptyDB :: DB
emptyDB = DB (Id 0) []

data TxId
  = TxId Id
  | TxVar Var
  deriving Show

data TxQ = TxQ { id :: TxId, attr :: Symbol, val :: Value, truth :: Bool }
  deriving Show

data Tx = Tx [TxQ]

data Var = Var String | Wildcard
  deriving (Eq, Show)

data Env a = Env [(String, a)]
  deriving Show

emptyEnv :: Env a
emptyEnv = Env []

lookupEnv :: Var -> Env a -> Maybe a
lookupEnv _ (Env []) = Nothing
lookupEnv Wildcard _ = Nothing
lookupEnv (Var x) (Env ((x', res) : env))
  | x == x' = Just res
  | otherwise = lookupEnv (Var x) (Env env)

insertEnv :: Var -> a -> Env a -> Env a
insertEnv Wildcard val env = env
insertEnv (Var x) val (Env env) = Env ((x, val) : env)

realizeTxQ :: Env Id -> Id -> Id -> TxQ -> (Q, Env Id, Id)
realizeTxQ env txId nextId (TxQ (TxId x) s v t) = (Q x s v txId t, env, nextId)
realizeTxQ env txId nextId (TxQ (TxVar x) s v t)
  | Just x' <- lookupEnv x env = (Q x' s v txId t, env, nextId)
  | otherwise = (Q nextId s v txId t, insertEnv x nextId env, incId nextId)

validTxId :: Id -> TxId -> Bool
validTxId nextId (TxId x) = x < nextId
validTxId _ _ = True

validValue :: Id -> Value -> Bool
validValue nextId (VId x) = x < nextId
validValue _ _ = True

validTxQ :: DB -> TxQ -> Bool
validTxQ (DB nextId _) (TxQ x _ v _) = validTxId nextId x && validValue nextId v

transact :: Tx -> DB -> Maybe DB
transact (Tx txQs) (DB nextId qs)
  | and (validTxQ (DB nextId qs) <$> txQs)
  , (newQs, nextId') <- tx emptyEnv nextId (incId nextId) [] txQs = Just (DB nextId' (sort (newQs ++ qs)))
  | otherwise = Nothing
  where
    tx :: Env Id -> Id -> Id -> [Q] -> [TxQ] -> ([Q], Id)
    tx env txId nextId newQs [] = (newQs, nextId)
    tx env txId nextId newQs (txQ : txQs)
      | (q, env', nextId') <- realizeTxQ env txId nextId txQ = tx env' txId nextId' (q : newQs) txQs

data DVal a
  = DVal a
  | DVar Var
  deriving (Eq, Show)

data DatalogQ = DatalogQ { id :: DVal Id, attr :: DVal Symbol, val :: DVal Value, tx :: DVal Id }
  deriving Show

data Query = Query [DatalogQ]
  deriving Show

unify :: Eq a => (a -> Value) -> (Value -> Maybe a) -> Env Value -> DVal a -> a -> Maybe (Env Value)
unify toVal fromVal env (DVal a) a'
  | a == a' = Just env
  | otherwise = Nothing
unify toVal fromVal env (DVar x) a
  | Just a' <- lookupEnv x env >>= fromVal
  , a == a' = Just env
  | Nothing <- lookupEnv x env = Just (insertEnv x (toVal a) env)
  | otherwise = Nothing

unifyId :: Env Value -> DVal Id -> Id -> Maybe (Env Value)
unifyId =
  unify
    VId
    (\res ->
      case res of
        VId id -> Just id
        _ -> Nothing
    )

unifyAttr :: Env Value -> DVal Symbol -> Symbol -> Maybe (Env Value)
unifyAttr =
  unify
    VSymbol
    (\res ->
      case res of
        VSymbol id -> Just id
        _ -> Nothing
    )

unifyValue :: Env Value -> DVal Value -> Value -> Maybe (Env Value)
unifyValue =
  unify
    (\x -> x)
    Just

unifyQ :: Env Value -> DatalogQ -> Q -> Maybe (Env Value)
unifyQ env dq q = do
  env <- unifyId env dq.id q.id
  env <- unifyAttr env dq.attr q.attr
  env <- unifyValue env dq.val q.val
  env <- unifyId env dq.tx q.tx
  return env

qStillHolds :: Q -> [Q] -> Bool
qStillHolds _ [] = True
qStillHolds q (q' : qs)
  | (q.id, q.attr, q.val) == (q'.id, q'.attr, q'.val) = False
  | otherwise = qStillHolds q qs

datalogQQuery :: Env Value -> DatalogQ -> DB -> [Env Value]
datalogQQuery _ _ (DB _ []) = []
datalogQQuery env dq (DB nid (q : qs))
  | q.truth == True
  , Just env' <- unifyQ env dq q
  , qStillHolds q qs = env' : datalogQQuery env dq (DB nid qs)
  | otherwise = datalogQQuery env dq (DB nid qs)

datalogQuery :: Env Value -> Query -> DB -> [Env Value]
datalogQuery env (Query []) _ = [env]
datalogQuery env (Query (e : es)) db = do
  env' <- datalogQQuery env e db
  datalogQuery env' (Query es) db

data Result = Result [Env Value]

val2Sexpr :: Value -> Sexpr
val2Sexpr (VId (Id x)) = Int x
val2Sexpr (VInt n) = Int n
val2Sexpr (VString s) = String s
val2Sexpr (VSymbol (Symbol s)) = Atom s

instance Show Result where
  show (Result envs) = show (List ((\(Env vars) -> List ((\(x, val) -> List [Atom x, val2Sexpr val]) <$> vars)) <$> envs))

query :: Query -> DB -> Result
query q db = Result (datalogQuery (Env []) q db)

pValue :: Sexpr -> Maybe Value
pValue (String s) = Just (VString s)
pValue (Int n) = Just (VInt n)
pValue _ = Nothing

pAttr :: Sexpr -> Maybe Symbol
pAttr (String s) = Just (Symbol s)
pAttr _ = Nothing

pBool :: Sexpr -> Maybe Bool
pBool (Bool b) = Just b
pBool _ = Nothing

pId :: Sexpr -> Maybe Id
pId (Int x) = Just (Id x)
pId _ = Nothing

parseQuery :: String -> Maybe Query
parseQuery s = runParser sexprP s >>= pDE
  where
    pDE :: Sexpr -> Maybe Query
    pDE (List qs) = Query <$> sequence (map pDQ qs)
    pDE _ = Nothing
    pDQ :: Sexpr -> Maybe DatalogQ
    pDQ (List [idS, attrS, valS, timeS]) =
      DatalogQ
        <$> pDV pId idS
        <*> pDV pAttr attrS
        <*> pDV pValue valS
        <*> pDV pId timeS
    pDV :: (Sexpr -> Maybe a) -> Sexpr -> Maybe (DVal a)
    pDV _ (Atom "_") = Just (DVar Wildcard)
    pDV _ (Atom x) = Just (DVar (Var x))
    pDV p s = DVal <$> p s

parseTx :: String -> Maybe Tx
parseTx s = runParser sexprP s >>= pTx
  where
    pTx :: Sexpr -> Maybe Tx
    pTx (List txQS) = Tx <$> sequence (map pTxQ txQS)
    pTxQ :: Sexpr -> Maybe TxQ
    pTxQ (List [txIdS, attrS, valS, truthT]) =
      TxQ
        <$> pTxId txIdS
        <*> pAttr attrS
        <*> pValue valS
        <*> pBool truthT
    pTxQ _ = Nothing
    pTxId :: Sexpr -> Maybe TxId
    pTxId (Atom "_") = Just (TxVar Wildcard)
    pTxId (Atom s) = Just (TxVar (Var s))
    pTxId s = TxId <$> pId s
