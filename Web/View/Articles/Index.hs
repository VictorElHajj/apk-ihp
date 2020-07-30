module Web.View.Articles.Index where
import Web.View.Prelude
import qualified Data.Text as Text

data IndexView = IndexView { articles :: [Article] }

instance View IndexView ViewContext where
    html IndexView { .. } = [hsx|
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
    <tr style="transform: rotate(0);">
        <th scope="row">{order}</th>
        <td>{get #apk article |> show |> Text.take 4}</td>
        <td><a href="https://www.systembolaget.se/{get #originId article}" class="stretched-link text-dark text-decoration-none">{get #name article}</a></td>
        <td>{get #itemGroup article ++ " "} <i class="text-secondary">{get #style article}</i></td>
        <td>{get #abv article |> ppFloat}%</td>
        <td>{((get #volume article)/10) |> ppFloat}cl</td>
        <td>{get #price article |> ppFloat}kr</td>
    </tr>
|]

ppFloat :: (RealFrac f, Ord f, Show f) => f -> Text
ppFloat f
    | r < 0.01 = show i
    | otherwise = show f
    where (i, r) = properFraction f
