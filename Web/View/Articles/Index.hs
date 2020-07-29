module Web.View.Articles.Index where
import Web.View.Prelude
import qualified Data.Text as Text

data IndexView = IndexView { articles :: [Article] }

instance View IndexView ViewContext where
    html IndexView { .. } = [hsx|
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
        <div class="table-responsive shadow">
            <table class="table table-striped table-hover table-borderless">
                <thead class="thead-dark">
                    <tr>
                        <th></th>
                        <th>APK</th>
                        <th>Namn</th>
                        <th>Typ</th>
                        <th>Alkohol</th>
                        <th>Volym</th>
                        <th>Pris</th>
                    </tr>
                </thead>
                <tbody>
                    {
                        let 
                            sortedArticles :: [(Int, Article)]
                            sortedArticles = articles 
                                |> sortBy (\a b -> compare (get #apk b) (get #apk a))
                                |> zip [1..]
                                |> take 100
                        in
                            forEach sortedArticles renderArticle
                    }
                </tbody>
            </table>
        </div>
    |]

renderArticle (order, article) = [hsx|
    <tr>
        <th scope="row">{order}</th>
        <td>{get #apk article |> show |> Text.take 4}</td>
        <td>{get #name article}</td>
        <td>{get #itemGroup article ++ " "} <i class="text-secondary">{get #style article}</i></td>
        <td>{get #abv article}%</td>
        <td>{(get #volume article)/10}cl</td>
        <td>{get #price article}kr</td>
    </tr>
|]
