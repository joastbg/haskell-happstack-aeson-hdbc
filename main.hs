module Main where

import Control.Monad    (msum)
import Data.Char        (toLower)
import Happstack.Server (FromReqURI(..), dirs, dir, path, seeOther, nullConf, simpleHTTP, toResponse, ok)
import Control.Monad.Trans
import Control.Monad
import System.IO
import Database.HDBC
import Database.HDBC.MySQL
import qualified Data.Text as T
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL

-- Representation of a person with firstname/lastname
data PersonName = 
     PersonName { firstName :: String
                , lastName  :: String
                } deriving (Eq, Show)

-- Converts SQL row into PersonName data
intoPersonName :: [SqlValue] -> PersonName
intoPersonName [firstName, lastName] =
    PersonName first last
    where first = (fromSql firstName) :: String
          last  = (fromSql lastName) :: String

-- Converts PersonName data into JSON
instance ToJSON PersonName where
    toJSON (PersonName fn ln) = object [ (T.pack "firstname") .= fn, (T.pack "lastname") .= ln ]

-- Extract one row from the database and converts it into JSON string
getFromDb :: Connection -> IO String
getFromDb conn = do 
    rows <- quickQuery' conn "SELECT * from persons" []
    let reply3 = head $ map intoPersonName rows
    return (BL.unpack $ encode reply3)

-- Fires up a happstack server and connects to a local MySQL server
main :: IO ()
main = do
    conn <- connectMySQL defaultMySQLConnectInfo {
        mysqlUser     = "testing",
        mysqlPassword = "testing1234",
        mysqlDatabase = "test"
    }
    str <- (getFromDb conn)
    simpleHTTP nullConf $ 
        msum [ dirs "db" $ ok str 
        , seeOther "db" "db"
        ]
