{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-wolfram-naira.awadin
-- File description:
-- Main
-}

import System.Environment
import System.Exit

data Conf = Conf {
    rule :: Maybe Rule,
    firstGen :: Int,
    linesDisp :: Maybe Int
}

type Rule = Int -> Int -> Int -> Int
rule30 :: Rule
rule30 1 1 1 = 0
rule30 1 1 0 = 0
rule30 1 0 1 = 0
rule30 1 0 0 = 1
rule30 0 1 1 = 1
rule30 0 1 0 = 1
rule30 0 0 1 = 1
rule30 0 0 0 = 0
rule30 _ _ _ = error "error"

rule90 :: Rule
rule90 1 1 1 = 0
rule90 1 1 0 = 1
rule90 1 0 1 = 0
rule90 1 0 0 = 1
rule90 0 1 1 = 1
rule90 0 1 0 = 0
rule90 0 0 1 = 1
rule90 0 0 0 = 0
rule90 _ _ _ = error "error"

rule110 :: Rule
rule110 1 1 1 = 0
rule110 1 1 0 = 1
rule110 1 0 1 = 1
rule110 1 0 0 = 0
rule110 0 1 1 = 1
rule110 0 1 0 = 1
rule110 0 0 1 = 1
rule110 0 0 0 = 0
rule110 _ _ _ = error "error"

getOpts :: Conf -> [String] -> Maybe Conf
getOpts conf [] = Just conf
getOpts conf ("--rule":ruleArg:args) =
    getOpts (conf {rule = parseRule ruleArg}) args
getOpts conf ("--start":firstArg:args) =
    if read firstArg < 0
        then Nothing
        else getOpts (conf {firstGen = read firstArg}) args
getOpts conf ("--lines":linesArg:args) =
    if read linesArg < 0
        then Nothing
        else getOpts (conf {linesDisp = Just (read linesArg)}) args
getOpts _ _ = Nothing

parseRule :: String -> Maybe Rule
parseRule "30" = Just rule30
parseRule "90" = Just rule90
parseRule "110" = Just rule110
parseRule _ = Nothing

generateInitline :: Conf -> [Int]
generateInitline _ = replicate 80 0 ++ [1] ++ replicate 80 0

nextGen :: Conf -> [Int] -> [Int]
nextGen conf lst = map nextState (zip3 (last lst : lst)
    lst (tail lst ++ [head lst]))
  where
    nextState (x, y, z) = case rule conf of
      Just r -> r x y z
      Nothing -> error "This rule doesn't exist"


dispGen :: [Int] -> String
dispGen gen = formatState $ concatMap (\x -> if x == 1 then "*" else " ")
    (take 80 $ drop (length gen `div` 2 - 40) gen)

formatState :: String -> String
formatState str = replicate ((80 - length str) `div` 2) ' ' ++ str

runprog :: Conf -> IO ()
runprog conf =
    let numLines = case linesDisp conf of
                        Just n -> n
                        Nothing -> 10000000
        start = max 0 $ min 80 (firstGen conf)
        generations = take numLines $ drop start $ iterate (nextGen conf)
            (generateInitline conf)
    in mapM_ (putStrLn.dispGen) generations

main :: IO ()
main = do
    args <- getArgs
    let error_message = putStrLn "This way -> --rule 30 --start (nb) --lines(nb)"
        exiterr = exitWith (ExitFailure 84)
    if null args then error_message >> exiterr
        else case getOpts (Conf undefined 0 Nothing) args of
            Just conf -> case rule conf of
                    Nothing -> putStrLn "Rule not found" >> exiterr
                    _ -> runprog conf
            Nothing -> error_message >> exiterr
