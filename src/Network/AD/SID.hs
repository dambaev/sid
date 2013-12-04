{-# LANGUAGE BangPatterns #-}
module Network.AD.SID where

import System.Process
import System.Exit
import Data.Char
import Data.List.Utils
import Control.Concurrent

type SIDT = String

data SID = SIDUser SIDT
         | SIDGroup SIDT
    deriving (Eq, Show)
type SIDs = [SID]



getCurrentUserSID:: IO (Either String (String,SID))
getCurrentUserSID = do
    (!idnucode, !idnuout, !iderr) <- readProcessWithExitCode "id" ["-nu"] ""
    (!idcode, !idout, !iderr) <- readProcessWithExitCode "id" ["-u"] ""
    let uid = filter isDigit idout
        username = dropWhile isSpace $! filter (\x-> x /= '\r' && x /= '\n') idnuout
    if idcode /= ExitSuccess then return $! Left $! "id -u returned " ++ 
            iderr
        else do
            (!wbcode, !wbout, !wberr) <- readProcessWithExitCode "wbinfo" 
                -- ["--uid-to-sid=" ++ uid] ""
                [ "-n" ++ username ] ""
            if wbcode /= ExitSuccess
                then return $! Left $! "wbinfo returned " ++ wberr
                else return $! Right $! (username, SIDUser $!
                    filter (\x-> isDigit x || isAlpha x || x=='-') $! takeWhile (not . isSpace ) wbout)
    

getCurrentGroupsSIDs:: SID-> IO (Either String [SID])
getCurrentGroupsSIDs (SIDUser sid ) = do
    (!wbcode, !wbout, !wberr) <- readProcessWithExitCode "wbinfo" 
        ["--user-sids=" ++ sid] ""
    if wbcode /= ExitSuccess
        then return $! Left $! "wbinfo returned: " ++ wberr
        else do
            let !lined = lines wbout
            return $! Right $! map (SIDGroup) lined
getCurrentGroupsSIDs _ = return $! Left "must be SIDUser"
