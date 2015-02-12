{-# LANGUAGE OverloadedStrings #-}

module Models.Shoes
	( get
	, list
	, add
	, testShoe
	, shoeForm
	, Shoe(..)
	) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (writeFile)
import Data.ByteString.Base64 (decode)
import Data.Int
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Prelude hiding (id)
import Snap.Snaplet.PostgresqlSimple

import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Text.Digestive

{----------------------------------------------------------------------------------------------------{
                                                                       | Records
}----------------------------------------------------------------------------------------------------}

data Shoe = Shoe
	{ id :: Int64
	, description :: Text
	, color :: Text
	, size :: Int64
	} deriving (Show, Eq)

instance FromRow Shoe where
	fromRow = Shoe <$> field <*> field <*> field <*> field

data NewShoe = NewShoe
	{ description' :: Text
	, color' :: Text
	, size' :: Int64
	, photo' :: ByteString
	}

testShoe :: NewShoe
testShoe = NewShoe "SADIE Faux Suede Heels with Bow" "red" 35 "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAMCAgMCAgMDAwMEAwMEBQgFBQQEBQoHBwYIDAoMDAsKCwsNDhIQDQ4RDgsLEBYQERMUFRUVDA8XGBYUGBIUFRT/2wBDAQMEBAUEBQkFBQkUDQsNFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBT/wAARCAA2ACUDASIAAhEBAxEB/8QAGwABAQACAwEAAAAAAAAAAAAAAAkGBwEFCAr/xAAxEAABAwMDAgUCBAcAAAAAAAABAgMEBQYRAAchCBIJEyIxUTJhF0GCkSMzQlJxgaH/xAAaAQABBQEAAAAAAAAAAAAAAAAAAgMEBQcG/8QAKxEAAQMDAgMHBQAAAAAAAAAAAQACAwQFERIhIjFxBhMyQVFhgaHB0eHx/9oADAMBAAIRAxEAPwCqClJQMqISPknXPGpgeKRvTNuC7U7a0+oPRaVTGG35jbKyA9KWAtPdj3CElGPglWs18N/ruO4UWHtTuJPIuyKnyaRVJCuai2kfyXFH3eSBwT9YGD6h6mhICcLpKqxVFLRw1bjnvBqx6D+YPQ+xVCuPjTj40xpjTq5tONNMaaEKSviRbJ1qxt3pd5+WqTb1xL8xmTyfKfCfWyo/keO5Pyk8fSceHno1RolYj1ykyHYEyM+mTGlMEpU08ghQUlQ9iDj/AJq3XiFW8mv9LtwKUgKXCmQZCCRkpJkttEj9LqtS3bsuVZN6XFt7c7PlS4z6h5CvoWsDlSPstHaoEe4xqvk4HEBbXaqxl5tbYp/HHt8AAA/UZ/aq10T9TUfqe2Zh1qQW2rpppEGuREcdr4HDoH5IcHqH5A9w/p1v/wDfUH+nPqCqnRrvy7Ukoen27KIi1enoIzJjE5S4gHjzEZ7h+pOQFHVtLD3YtHcyzIV123X4VSoMsJDctDwSErUQA2sHBQvJAKFYIJxjUuN+oLL7pQOo5yGjhPL8LLP300wTpp1Ui6K+bHo249qVG27hhmfR56AiRHDq2ioBQUMLQQpJCkgggg8anB4ou1kml3dQL8hxVR1ONCDKlMjAUpHLKyf7u3KT9ko1T3H21je4W3dB3StSbblyQG6jSpiO1xpfuPggjkEHkEcjSHsDhhXNpuLrZVsn5tGxHqDz+x6gL5/Kg63e1WpzNecDDaXAl2ayj+IGyecge/8Aof4B9tbAvna/8MaYmo27PcqNjTXEOImJX6mncHsRISDjux3dqx6VDPbg9yR7wvjwmLSqSlu2zdtWpKlZwxMSiQ2PsOEq/cnWvZvhLXfPhsQHNzWDT2HFOtsrgLUkKPurt80DP31BMMgIwdlpMfaW2RytqYTpd5gt8uozg/K9PeH1uBeO4uxDU+7JaaoyxKMal1IvIcekR0oTkOkKKu9CypHrCVYSD6shRa7/AKP+lxXSxZVWoi7nkXM7UpglrUqMIzTJCAnCG+9fJxyrPOE8DHLU9uQBlZhc5Yp6yWWHGlxyMDA39lvzA0wNNNKVYnGuONNNCEOmmmhC/9k="

{----------------------------------------------------------------------------------------------------{
                                                                       | Forms
}----------------------------------------------------------------------------------------------------}
{-
this is what our form would look like if we were editing it, but it is missing the image field
shoeForm :: Monad m => Maybe Shoe -> Form Text m Shoe
shoeForm s = Shoe
	<$> "id" .: stringRead "Must be a number" (Just $ maybe 0 id s)
	<*> "description" .: text (description <$> s)
	<*> "color" .: text (color <$> s)
	<*> "size" .: stringRead "Must be a number" (size <$> s)
-}

shoeForm :: Monad m => Form Text m NewShoe
shoeForm = NewShoe
	<$> "description" .: check "Cannot be blank" (/= "") (text Nothing)
	<*> "color" .: check "Cannot be blank" (/= "") (text Nothing)
	<*> "size" .: check "Cannot be blank" (/= 0) (stringRead "Not a valid number" Nothing)
	<*> "photo" .: validate validPhoto (text Nothing)
	where
		-- NOTE: I'm trusting that a JPG is being submitted, normally I'd check the file contents to ensure a valid image
		validPhoto "" = Error "Cannot be blank"
		validPhoto x = case decode $ encodeUtf8 x of
			Left x' -> Error $ pack x'
			Right x' -> Success x'

{----------------------------------------------------------------------------------------------------{
                                                                       | Queries
}----------------------------------------------------------------------------------------------------}

list :: HasPostgres m => m [Shoe]
list = query_ "SELECT id, description, color, size FROM shoes ORDER BY description, color, size"

get :: (HasPostgres m, Functor m) => Int64 -> m (Maybe Shoe)
get i = listToMaybe <$> query "SELECT id, description, color, size FROM shoes WHERE id = ?" (Only i)

-- TODO: fix type signature
--add :: (HasPostgres m, Functor m) => m (Either ConstraintViolation Int64)
add :: (HasPostgres m, Functor m) => NewShoe -> m (Maybe Int64)
add s = do
	{-
	A transaction should be used here so we can roll it back if writing the image out to the filesystem fails.
	Currently snap-postgresql-simple doesn't work with transactions, but if we were going to do that, this
	solves that issue:  https://github.com/mightybyte/snaplet-postgresql-simple/pull/8
	-}
	r <- listToMaybe <$> query "INSERT INTO shoes (description, color, size) VALUES (?, ?, ?) RETURNING id" (description' s, color' s, size' s)
	let i = fromOnly <$> r
	liftIO $ case i of
		Just i' -> B.writeFile ("static/uploads/" <> show i' <> ".jpg") $ photo' s
		_ -> print "Failed to insert the shoe"
	return i
