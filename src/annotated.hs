module Annotated where

import Control.Exception.Annotated
import qualified Control.Exception

import GHC.Stack

newtype MyException = MyException String
    deriving Show

instance Exception MyException

boom :: IO Int
boom = throw (MyException "boom")

recovery :: IO Int
recovery =
    boom `catch` \(MyException message) -> do
        putStrLn message
        throw (MyException (message ++ " recovered"))

recoveryOther :: IO Int
recoveryOther =
    boom `catch` \(MyException message) -> do
        putStrLn message
        throw (OtherException (length message))

newtype OtherException = OtherException Int
    deriving Show

instance Exception OtherException

recoveryAnnotated :: IO Int
recoveryAnnotated =
    boom `catch` \(AnnotatedException annotations (MyException message)) -> do
        putStrLn message
        traverse print annotations
        throw (OtherException (length message))

recoveryAnnotatedPreserve :: IO Int
recoveryAnnotatedPreserve =
    boom `catch` \(AnnotatedException annotations (MyException message)) -> do
        putStrLn message
        traverse print annotations
        throw (AnnotatedException annotations (OtherException (length message)))

emptyAnnotationsAreCool :: IO ()
emptyAnnotationsAreCool =
    Control.Exception.throwIO (MyException "definitely not annotated?")
        `Control.Exception.catch`
            \(AnnotatedException annotations (MyException woah)) -> do
                print annotations
                putStrLn woah

catchPutsACallStack :: IO ()
catchPutsACallStack =
    Control.Exception.throwIO (MyException "definitely not annotated?")
        `catch`
            \(MyException woah) -> do
                throw (OtherException (length woah))
