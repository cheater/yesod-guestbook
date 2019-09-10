{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import           Text.Julius             (RawJS (..))
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    -- (formWidget, formEnctype) <- generateFormPost sampleForm
    let -- submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text
    -- allComments <- runDB $ getAllComments
    allGuests <- runDB $ getAllGuests

    defaultLayout $ do
        let
            -- (commentFormId, commentTextareaId, commentListId) = commentIds
            ( guestbookFormId
              , guestbookNameInputId
              , guestbookEmailInputId
              , guestbookListId
              ) = guestbookIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    -- ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        -- submission = case result of
        --     FormSuccess res -> Just res
        --     _ -> Nothing
    -- allComments <- runDB $ getAllComments
    allGuests <- runDB $ getAllGuests

    defaultLayout $ do
        let
            -- (commentFormId, commentTextareaId, commentListId) = commentIds
            ( guestbookFormId
              , guestbookNameInputId
              , guestbookEmailInputId
              , guestbookListId
              ) = guestbookIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

-- sampleForm :: Form FileForm
-- sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
--     <$> fileAFormReq "Choose a file"
--     <*> areq textField textSettings Nothing
--     -- Add attributes like the placeholder and CSS classes.
--     where textSettings = FieldSettings
--             { fsLabel = "What's on the file?"
--             , fsTooltip = Nothing
--             , fsId = Nothing
--             , fsName = Nothing
--             , fsAttrs =
--                 [ ("class", "form-control")
--                 , ("placeholder", "File description")
--                 ]
--             }
--
-- commentIds :: (Text, Text, Text)
-- commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
--
-- getAllComments :: DB [Entity Comment]
-- getAllComments = selectList [] [Asc CommentId]

guestbookIds :: (Text, Text, Text, Text)
guestbookIds =
    ( "js-guestbookForm"
    , "js-guestbookNameInput"
    , "js-guestbookEmailInput"
    , "js-guestbookList"
    )

getAllGuests :: DB [(E.Value Text, E.Value Text, E.Value Text)]
getAllGuests =
      E.select
    $ E.from $ \(guest `E.InnerJoin` user) -> do
        E.on $ guest ^. GuestCreator E.==. user ^. UserId
        E.orderBy [E.desc $ guest ^. GuestId]
        return
            ( guest ^. GuestName
            , guest ^. GuestEmail
            , user  ^. UserIdent
            )
