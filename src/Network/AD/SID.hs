{-# LANGUAGE BangPatterns #-}
module Network.AD.SID where

import System.Process
import System.Exit
import Data.Char

type SIDT = String

data SID = SIDUser SIDT
         | SIDGroup SIDT
    deriving (Eq, Show)
type SIDs = [SID]



getCurrentUserSID:: IO (Either String SID)
getCurrentUserSID = do
    (!idcode, !idout, !iderr) <- readProcessWithExitCode "id" ["-u"] ""
    putStrLn $! idout
    putStrLn $! iderr
    let uid = filter isDigit idout
    if idcode /= ExitSuccess then return $! Left $! "id -u returned " ++ 
            show idcode
        else do
            (!wbcode, !wbout, !wberr) <- readProcessWithExitCode "wbinfo" 
                ["--uid-to-sid=" ++ uid] ""
            putStrLn $! wbout
            putStrLn $! wberr
            if wbcode /= ExitSuccess
                then return $! Left $! "wbinfo returned " ++ show wbcode
                else return $! Right $! SIDUser $!
                    filter (\x-> isDigit x || isAlpha x || x=='-') wbout
    

getCurrentGroupsSIDs:: IO [SID]
getCurrentGroupsSIDs = undefined
