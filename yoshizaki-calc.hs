{-
段階ごとの構文に対する責務が散らばっている
木を作らずにリストのまま少しづつ短くするような流れ
意思が弱いのでMaybe多用
Either?Exception?知らないですね -> ゆえにエラーメッセージが貧弱
Haskellの基礎から取り組んで3日目には夢にHaskellが出てた
Haskell警察が来てもエディタの前で副作用万歳と叫ぶ
-}
module Main where

import Data.Char
import Data.IORef
import Data.List
import Data.String.Utils
import Data.Map
import System.IO

import Debug.Trace

type VTable = Map String Int     -- 変数のあれ


isUnaryOperator c = c == '+' || c == '-'    -- 単項オペレーター
isBrackets c = c == '(' || c == ')'
isOperator c = c == '*' || c == '/' || c == '%'
isEqOperator c = c == '='


data Token = UnaryOperatorToken Char
        | BracketsToken Char
        | OperatorToken Char
        | NumberToken Int
        | VariableToken String
        | SubstitutionToken
            deriving (Eq, Show) 

isUnaryOperatorToken token = case token of UnaryOperatorToken _ -> True; _ -> False;
isBracketsToken token = case token of BracketsToken _ -> True; _ -> False;
isOperatorToken token = case token of OperatorToken _ -> True; _ -> False;
isNumberToken token = case token of NumberToken _ -> True; _ -> False;


makeTokenElm :: String -> Token
makeTokenElm (x:xs)
    | isUnaryOperator x = if mod (length (Prelude.filter (=='-') (x:xs))) 2 == 0
                                    then UnaryOperatorToken '+' 
                                    else UnaryOperatorToken '-'
    | isBrackets x      = BracketsToken x
    | isOperator x      = OperatorToken x
    | isNumber x        = NumberToken (read (x:xs) :: Int)
    | isAlpha x         = VariableToken (x:xs)
    | isEqOperator x    = SubstitutionToken
    where
        ehead = head xs
        elast = last xs

-- 分割　（ゝω・）
tokenize line = impl (replace " " "" line) [] []
    where
        impl :: String -> String -> [Token] -> Maybe [Token]
        impl [] [] [] = Just []
        impl [] workbuffer tokens = Just $ tokens ++ [makeTokenElm workbuffer]
        impl (x:xs) [] []
            | isUnaryOperator x = impl xs [x] []
            | isBrackets x      = if x == ')' then Nothing else impl xs [x] []
            | isOperator x      = impl xs [x] []
            | isEqOperator x    = Nothing
            | isNumber x        = impl xs [x] []
            | isAlpha x         = impl xs [x] []
            | otherwise         = Nothing
        impl (x:xs) workbuffer tokens
            | isUnaryOperator x = if isUnaryOperator buflast
                                        then impl xs (workbuffer ++ [x]) tokens
                                        else impl xs [x] (tokens ++ [makeTokenElm workbuffer])
            | isBrackets x      = if isNumber x || isAlpha x || isBrackets x
                                        then impl xs [x] (tokens ++ [makeTokenElm workbuffer])
                                        else Nothing
            | isOperator x      = if isAlpha buflast || isNumber buflast || isBrackets buflast
                                        then impl xs [x] (tokens ++ [makeTokenElm workbuffer])
                                        else Nothing
            | isEqOperator x    = if isNumber bufhead || isAlpha bufhead
                                        then impl xs [x] (tokens ++ [makeTokenElm workbuffer])
                                        else Nothing
            | isNumber x        = if isNumber buflast || isAlpha bufhead
                                        then impl xs (workbuffer ++ [x]) tokens
                                        else impl xs [x] (tokens ++ [makeTokenElm workbuffer])
            | isAlpha x         = if isAlpha buflast
                                        then impl xs (workbuffer ++ [x]) tokens
                                        else if isNumber bufhead
                                                then Nothing
                                                else impl xs [x] (tokens ++ [makeTokenElm workbuffer])
            | otherwise         = Nothing
            where
                bufhead = head workbuffer
                buflast = last workbuffer


debug = flip trace


getBracketsPairIdx :: [Token] -> Maybe [Int]
getBracketsPairIdx tokens = do
    let impl (t:ts) nStart nEnd startIdx count
            | t == BracketsToken '(' = if nStart == 0
                                            then impl ts 1 nEnd count (count + 1)
                                            else impl ts (nStart + 1) nEnd startIdx (count + 1)
            | t == BracketsToken ')' = if nEnd + 1 == nStart
                                            then Just [startIdx, count]
                                            else if nEnd + 1 > nStart
                                                then Nothing
                                                else impl ts nStart (nEnd + 1) startIdx (count + 1)
            | otherwise              = impl ts nStart nEnd startIdx (count + 1)

        impl [] _ _ _ _ = Nothing

    impl tokens 0 0 0 0


shrinkTokens :: [Token] -> Maybe Token
shrinkTokens []       = Nothing
shrinkTokens [t]      = if isNumberToken t then Just t else Nothing
shrinkTokens [t1, t2] = Nothing -- Wrong
shrinkTokens [_,BracketsToken x,_] = Nothing
shrinkTokens [_,NumberToken x,_] = Nothing
shrinkTokens [_,SubstitutionToken,_] = Nothing
shrinkTokens tokens = do
    Just (NumberToken 3) `debug` show tokens
    -- ()
    case getBracketsPairIdx tokens of
        Nothing ->
            -- ペアになっていない括弧があるか先頭と最後尾が数字でない場合は不正
            if Data.List.null (findIndices isBracketsToken tokens)
                && (isNumberToken (head tokens) || isNumberToken (last tokens))
                then do
                    let opIdx = Data.List.findIndex isOperatorToken tokens
                    case opIdx of
                        Nothing  -> do
                            -- +-
                            -- Expected head and tail is Number
                            let (t1:t2:t3:ts) = tokens
                            let (NumberToken val1) = t1
                            let (NumberToken val2) = t3
                            let (UnaryOperatorToken op) = t2

                            let ret = case op of
                                    '+' -> val1 + val2
                                    '-' -> val1 - val2

                            shrinkTokens $ NumberToken ret : ts
     
                        Just idx -> do
                            let (NumberToken val1) = tokens !! (idx-1)
                            let (NumberToken val2) = tokens !! (idx+1)
                            let (OperatorToken op) = tokens !! idx

                            let ret = case op of
                                    '*' -> Just (val1 * val2)
                                    '/' -> case val2 of
                                            0 -> Nothing
                                            _ -> Just (div val1 val2)
                                    '%' -> Just (mod val1 val2)

                            case ret of
                                Nothing -> Nothing
                                Just num -> shrinkTokens $ take (idx-1) tokens ++ [NumberToken num] ++ drop (idx + 2) tokens
        
                else Nothing

        Just pair -> do
            let i1 = head pair
            let i2 = last pair
            let child = shrinkTokens (drop (i1+1) (take i2 tokens))

            case child of
                Nothing -> Nothing
                Just x  -> shrinkTokens $ take i1 tokens ++ [x] ++ drop (i2+1) tokens


applyVariable :: [Token] -> VTable -> Maybe [Token]
applyVariable tokens vTable = impl tokens vTable []
    where
        impl :: [Token] -> VTable -> [Token] -> Maybe [Token]
        impl [] _ ret     = Just ret
        impl (x:xs) vTable ret =
            case x of
                VariableToken key ->
                    case Data.Map.lookup key vTable of
                        Just num -> impl xs vTable (ret ++ [NumberToken num])
                        Nothing  -> Nothing                    
                _                  -> impl xs vTable (ret ++ [x])

-- -3 を 0 - 3, (-3を ( -3 的な
-- 原罪を背負っているコード
normalizeUnaryOperator :: [Token] -> Maybe [Token]
normalizeUnaryOperator tokens = impl tokens []
    where
        impl :: [Token] -> [Token] -> Maybe [Token]
        impl [] [] = Nothing
        impl [t] [] = Just [t]

        impl [t1@(UnaryOperatorToken uop), t2@(NumberToken num)] [] = Just $ if uop == '+' then [t2] else [NumberToken ((-1) * num)]
        impl [t1, t2] [] = Nothing

        impl (t1@(UnaryOperatorToken uop):t2:tx) [] = impl (t2:tx) [NumberToken 0,t1]
        impl (t1:t2:tx) [] = impl (t2:tx) [t1]

        impl [t] ret = impl [] (ret ++ [t])

        impl [t1@(UnaryOperatorToken uop), t2@(NumberToken num)] ret = case last ret of
                (BracketsToken op) | op == ')'  -> impl [] (ret ++ [t1] ++ [t2])
                                   | uop == '+' -> impl [] (ret ++ [t2])
                                   | otherwise  -> impl [] (ret ++ [NumberToken ((-1) * num)])
                (OperatorToken op) | uop == '+' -> impl [] (ret ++ [t2])
                                   | otherwise  -> impl [] (ret ++ [NumberToken ((-1) * num)])
                _ -> impl [] (ret ++ [t1] ++ [t2])
        impl [t1,t2] ret = impl [] (ret ++ [t1] ++ [t2])

        impl (t1@(UnaryOperatorToken uop):t2@(NumberToken num):tx) ret = case last ret of
                (BracketsToken op) | op == ')'  -> impl tx (ret ++ [t1] ++ [t2])
                                   | uop == '+' -> impl tx (ret ++ [t2])
                                   | otherwise  -> impl tx (ret ++ [NumberToken ((-1) * num)])
                (OperatorToken op) | uop == '+' -> impl tx (ret ++ [t2])
                                   | otherwise  -> impl tx (ret ++ [NumberToken ((-1) * num)])
                _ -> impl tx (ret ++ [t1] ++ [t2])
        impl (t1:t2:tx) ret = impl (t2:tx) (ret ++ [t1])

        impl _ ret = Just ret   -- やったぜ



calculate :: [Token] -> VTable -> Maybe Int
calculate tokens variableTable = do
    variableApplied <- applyVariable tokens variableTable
    normalizedUnary <- normalizeUnaryOperator variableApplied
    case shrinkTokens normalizedUnary of
        Just (NumberToken n) -> Just n
        _ -> Nothing

substitution :: [Token] -> IORef VTable -> IO()
substitution (VariableToken x:SubstitutionToken:xs) variableTableRef = do
    vTable <- readIORef variableTableRef
    case calculate xs vTable of
        Just num -> writeIORef variableTableRef (Data.Map.insert x num vTable)
        Nothing  -> putStrLn "無理"
substitution _ _ = putStrLn "代入無理"

processToken :: [Token] -> IORef VTable -> IO()
processToken tokens variableTableRef = do
    vTable <- readIORef variableTableRef
    case numEq of
        0 -> case calculate tokens vTable of
                Nothing -> putStrLn "無理"
                Just v  -> print v
        1 -> substitution tokens variableTableRef
        _ -> return ()
    where
        numEq = length [x | x <- tokens, case x of SubstitutionToken -> True; _ -> False;]


-- 副作用万歳(/・ω・)/
parseAndRun :: String -> IORef VTable -> IO()
parseAndRun line variableTableRef =
    case tokens of
        Nothing -> putStrLn "無理"
        Just ts -> processToken ts variableTableRef
    where
        tokens = tokenize line

app :: IORef VTable -> IO()
app variableTableRef = do
    putStr "> "
    line <- getLine
    parseAndRun line variableTableRef
    app variableTableRef

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    variableTableRef <- newIORef (fromList [] :: VTable)
    app variableTableRef
