{-# LANGUAGE OverloadedStrings #-}
module Handler.Guestbook where

import Import

postGuestbookR :: Handler Value
postGuestbookR = do
    liftIO $ Prelude.putStrLn "starting postGuestbookR"
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    guestbook <- (requireJsonBody :: Handler Guest)
    liftIO $ Prelude.putStrLn ("parsed json body.")
    -- liftIO $ Prelude.putStrLn ("json body: " Prelude.++ (show guestbook))

    -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
    maybeCurrentUserId <- maybeAuthId
    liftIO $ Prelude.putStrLn $ show maybeCurrentUserId
    case maybeCurrentUserId of
        Nothing -> permissionDenied "Can't add guests if not logged in."
        Just currentUserId -> do
            let guestbook' = guestbook { guestCreator = currentUserId }

            insertedGuestbook <- runDB $ insertEntity guestbook'
            returnJson insertedGuestbook
