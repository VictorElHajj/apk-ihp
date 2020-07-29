module Web.Routes where
import IHP.RouterPrelude
import Generated.Types
import Web.Types

-- Generator Marker
instance AutoRoute ArticlesController
type instance ModelControllerMap WebApplication Article = ArticlesController

