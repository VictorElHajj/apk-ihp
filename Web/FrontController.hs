module Web.FrontController where
import IHP.RouterPrelude
import IHP.ControllerSupport
import Generated.Types
import Web.Types

-- Controller Imports
import Web.Controller.Articles
import IHP.Welcome.Controller

instance FrontController WebApplication where
    controllers = 
        [ startPage WelcomeAction
        -- Generator Marker
        , parseRoute @ArticlesController
        ]

instance InitControllerContext WebApplication
