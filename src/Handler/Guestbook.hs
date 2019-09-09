{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Guestbook where

import Import
import Data.HashMap.Strict as H (lookup)

postGuestbookR :: Handler Value
postGuestbookR = do
    liftIO $ Prelude.putStrLn "starting postGuestbookR"
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    guestbook <- (requireJsonBody :: Handler Value)
    liftIO $ Prelude.putStrLn ("parsed json body.")
    liftIO $ Prelude.putStrLn ("json body: " Prelude.++ (show guestbook))

    -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
    maybeCurrentUserId <- maybeAuthId
    liftIO $ Prelude.putStrLn $ show maybeCurrentUserId
    case maybeCurrentUserId of
        Nothing -> permissionDenied "Can't add guests if not logged in."
        Just currentUserId -> do
            case guestbook of
                Object guestbookH -> do
                    liftIO $ Prelude.putStrLn "guestbook':"
                    liftIO $ Prelude.putStrLn $ (show guestbookH)
                    let
                        mName = H.lookup "name" guestbookH
                        mEmail = H.lookup "email" guestbookH
                    case (mName, mEmail) of
                        (Just (String name), Just (String email)) -> do
                            let guest = Guest { guestEmail = email, guestName = name, guestCreator = currentUserId }
                            insertedGuest <- runDB $ insertEntity guest
                            returnJson insertedGuest

                        _ -> invalidArgs ["Invalid JSON payload"]

                _ -> invalidArgs ["Invalid JSON payload"]
