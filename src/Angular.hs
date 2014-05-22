{-# LANGUAGE OverloadedStrings #-}
module Angular where

import           Text.Blaze.Html5
import qualified Text.Blaze.Html5   as H
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import           Data.Text (Text)
import           Text.Blaze
import           Text.Blaze.Internal

-- These need to be defined with TH!
ngModel      = customAttribute "ng-model"
ngIf         = customAttribute "ng-if"
ngHide       = customAttribute "ng-hide"
ngController = customAttribute "ng-controller"
ngApp        = customAttribute "ng-app"
ngClass      = customAttribute "ng-class"
ngShow       = customAttribute "ng-show"
ngRepeat     = customAttribute "ng-repeat"
ngClick      = customAttribute "ng-click"
ngScope      = customAttribute "ng-scope"
ngCloak      = customAttribute "ng-cloak"
ngSrc        = customAttribute "ng-src"
ngDisabled   = customAttribute "ng-disabled"
ngBlur       = customAttribute "ng-blur"

ngView = elemDirective "ng-view"

-- | Copping out and returning String to simplify the mess of needing to
-- convert both to AttributeValue and Html.
template :: Text -> String
template expr = T.unpack $ T.concat ["{{", expr, "}}"]

-- | This function can, and in fact should, be used to create non-standard
-- html elements. Using it for purposes other than teaching blaze-html
-- about Angular directives [http://docs.angularjs.org/guide/directive]
-- shall be met with arched eyebrows and muttered invectives.
elemDirective :: Text -> Html -> Html
elemDirective tag = Parent tag' open close
    where
      tag'  = fromText tag
      open  = fromText $ T.concat ["<", tag]
      close = fromText $ T.concat ["</", tag, ">"]
      -- Inferred from IsString definition
      fromText t = StaticString ((T.unpack t) ++) (T.encodeUtf8 t) t
