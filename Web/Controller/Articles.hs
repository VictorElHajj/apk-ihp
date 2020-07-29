module Web.Controller.Articles where

import Web.Controller.Prelude
import Web.View.Articles.Index

instance Controller ArticlesController where
    action ArticlesAction = do
        articles <- query @Article |> fetch
        render IndexView { .. }
