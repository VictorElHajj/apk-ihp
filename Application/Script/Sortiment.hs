#!/usr/bin/env run-script
module Application.Script.Sortiment where
import Application.Script.Prelude
import Data.Maybe
import Data.Functor
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Read as Read
import qualified Network.HTTP.Simple as Net
import qualified Text.XML.Light as XML

run :: Script
run = do
    putStrLn "Downloading report."
    response <- Net.httpBS "https://www.systembolaget.se/api/assortment/products/xml" 
        <&> Net.getResponseBody
        <&> Encoding.decodeUtf8
    putStrLn "Parsing report."
    let xml = parseXML response
        articles = map buildArticle xml
    putStrLn "Deleting old records." 
    old <- query @Article |> fetch
    deleteRecords old
    putStrLn "Creating new records."
    createMany articles
    putStrLn "Complete."

parseXML :: Text -> [XML.Element]
parseXML file = file
            |> XML.parseXMLDoc
            |> \case
                Nothing -> error "Failed to parse XML"
                Just parsed -> parsed
                    |> XML.elChildren 
                    |> drop 2 -- Skips over info to get to articles

getArticleProperty :: Text -> XML.Element -> Text
getArticleProperty name element = element
    |> XML.findChild (XML.unqual (Text.unpack name))
    |> \case
        Nothing -> error ("Article does not have property " ++ name)
        Just child -> child
         |> XML.elContent
         |> head
         |> \case 
            (Just (XML.Text text)) -> text
                |> XML.cdData
                |> Text.pack
            _ -> "" -- This usually means the XML is like <Stil/> instead of <Stil>Style Example</Stil>, so "" is expected output

-- Like the gerTarticleProperty but intended for elements that do not show up in all articles, defaults to empty string.
getArticlePropertyMaybe :: Text -> XML.Element -> Text
getArticlePropertyMaybe name element = element
    |> XML.findChild (XML.unqual (Text.unpack name))
    |> \case
        Nothing -> "0.0" 
        Just child -> child
         |> XML.elContent
         |> head
         |> \case 
            (Just (XML.Text text)) -> text
                |> XML.cdData
                |> Text.pack
            _ -> "0.0" -- This usually means the XML is like <Stil/> instead of <Stil>Style Example</Stil>, so "" is expected output

buildArticle element = do
                        let getInt p = getArticleProperty p element 
                                |> Read.decimal
                                |> \case
                                    (Left e) -> "Failed to parse " ++ p ++ " as Int" |> error 
                                    (Right (v,_)) -> v
                            getFloat p = getArticleProperty p element 
                                |> Text.filter (/='%')
                                |> Read.double
                                |> \case
                                    (Left e) -> "Failed to parse " ++ p ++ " as Float" |> error 
                                    (Right (v,_)) -> realToFrac v
                            getMaybeFloat p = getArticlePropertyMaybe p element
                                |> Text.filter (/='%')
                                |> Read.double
                                |> \case
                                    (Left e) -> "Failed to parse " ++ p ++ " as Float" |> error 
                                    (Right (v,_)) -> realToFrac v
                            getText p = getArticleProperty p element
                            abv = getFloat "Alkoholhalt"
                            volume = getFloat "Volymiml" 
                            price = getFloat "Prisinklmoms" + getMaybeFloat "Pant"
                        newRecord @Article
                            |> set #originId (getInt "Artikelid")
                            |> set #name (getText "Namn" ++ " " ++ getText "Namn2")
                            |> set #price price 
                            |> set #volume volume 
                            |> set #itemGroup (getText "Varugrupp") 
                            |> set #style (getText "Stil")
                            |> set #abv abv 
                            |> set #availability (getText "SortimentText") 
                            |> set #apk (abv/100*volume/price)
