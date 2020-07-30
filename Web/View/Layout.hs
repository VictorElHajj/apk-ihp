module Web.View.Layout (defaultLayout, Html) where

import IHP.ViewPrelude
import IHP.Environment
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Types
import Web.Routes
import qualified IHP.FrameworkConfig as FrameworkConfig
import Config ()

type Html = HtmlWithContext ViewContext

defaultLayout :: Html -> Html
defaultLayout inner = H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    {metaTags}

    {stylesheets}
    {scripts}

    <title>APK</title>
</head>
<body>
    <div class="container mt-4">
        <nav class="navbar navbar-expand navbar-dark bg-dark shadow">
            <a class="navbar-brand" href="#">APK</a>
            <ul class="navbar-nav">
            <li class="nav-item active">
                <a class="nav-link" href="#">Hem <span class="sr-only">(current)</span></a>
            </li>
            <li class="nav-item">
                <a class="nav-link" href="#">Om</a>
            </li>
            </ul>
        </nav>
        {renderFlashMessages}
        {inner}
    </div>
</body>
|]


stylesheets = do
    when (isDevelopment FrameworkConfig.environment) [hsx|
        <link rel="stylesheet" href="/vendor/bootstrap.min.css"/>
        <link rel="stylesheet" href="/vendor/flatpickr.min.css"/>
        <link rel="stylesheet" href="/app.css"/>
    |]
    when (isProduction FrameworkConfig.environment) [hsx|
        <link rel="stylesheet" href="/prod.css"/>
    |]

scripts = do
    when (isDevelopment FrameworkConfig.environment) [hsx|
        <script id="livereload-script" src="/livereload.js"></script>
        <script src="/vendor/jquery-3.2.1.slim.min.js"></script>
        <script src="/vendor/timeago.js"></script>
        <script src="/vendor/popper.min.js"></script>
        <script src="/vendor/bootstrap.min.js"></script>
        <script src="/vendor/flatpickr.js"></script>
        <script src="/helpers.js"></script>
        <script src="/vendor/morphdom-umd.min.js"></script>
    |]
    when (isProduction FrameworkConfig.environment) [hsx|
        <script src="/prod.js"></script>
    |]


metaTags = [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    <meta property="og:title" content="Alkol per krona"/>
    <meta property="og:type" content="website"/>
    <meta property="og:url" content="TODO"/>
    <meta property="og:description" content="TODO"/>
|]
