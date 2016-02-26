{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
	( app
	) where

------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
--import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy as BL (toStrict)
import Data.Int
import Data.Text.Encoding (decodeUtf8)
import Data.Maybe (listToMaybe)
import Data.Monoid (mempty, (<>))
import qualified Data.Text as T
import Safe (readMay)
import Snap.Core hiding (getParam)
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.PostgresqlSimple
import Snap.Util.FileServe
import Heist
import Heist.Interpreted
--import Heist.SpliceAPI
--import Text.Digestive.Heist (digestiveSplices)

import Data.Aeson (json, encode)
import Data.Attoparsec.Lazy (parse, maybeResult)
import Text.Digestive.Aeson (digestJSON, jsonErrors)

------------------------------------------------------------------------------
import Application
import Splices

import qualified Models.Shoes as Shoes

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes =
	[ ("/", ifTop indexH)
	, ("/:id", intParamModel viewH Shoes.get "id")
	, ("/add", method POST addH)
	, ("", serveDirectory "static")
	]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
	h <- nestSnaplet "heist" heist $ heistInit' "templates" defaultHeistState
	d <- nestSnaplet "db" db pgsInit

	addRoutes routes
	return $ App h d
	where defaultHeistState = mempty
		{ hcInterpretedSplices = defaultInterpretedSplices
		, hcLoadTimeSplices = defaultLoadTimeSplices
		}

indexH :: AppHandler ()
indexH = do
	shoes <- Shoes.list
	let splices = "shoe" #! listToSplice shoeSplices shoes
	renderWithSplices "index" splices

viewH :: Shoes.Shoe -> AppHandler ()
viewH shoe =
	let splices = "shoe" #! runChildrenWith $ shoeSplices shoe
	in renderWithSplices "view" splices

addH :: AppHandler ()
addH =  do
	-- TODO: set a more appropriate limit on the requestBody
	-- TODO: refactor this into a reusable JSON parsing helper function
	parsedJson <- maybeResult . parse json <$> readRequestBody 1000000
	case parsedJson of
		Nothing -> writeBS "Invalid JSON object\n"
		Just x -> do
			(view, result) <- digestJSON Shoes.shoeForm x
			case result of
				Nothing -> writeBS $ BL.toStrict $ encode (jsonErrors view) <> "\n"
				Just r -> do
					_ <- Shoes.add r
					writeBS "Success\n"

------------------------------------------------------------------------------
-- | Helper functions

getParam :: ByteString -> Request -> Maybe ByteString
getParam name = listToMaybe <=< rqParam name

intParam :: MonadSnap m => ByteString -> m (Maybe Int64)
intParam name = withRequest $ \request -> do
	let p = getParam name request
	return $ (readMay . T.unpack . decodeUtf8) =<< p

intParamModel :: (a -> AppHandler ()) -> (Int64 -> AppHandler (Maybe a)) -> ByteString -> AppHandler ()
intParamModel renderer model key = do
	key' <- intParam key
	model' <- maybe (return Nothing) model key'
	case model' of
		Just x -> renderer x
		Nothing -> pass

