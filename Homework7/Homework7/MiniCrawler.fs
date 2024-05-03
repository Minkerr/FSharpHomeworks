module Homework7.MiniCrawler

open System.Text.RegularExpressions
open HtmlAgilityPack

//<a href="http://...">

let crawler (url : string)  = 
    let web = HtmlWeb()
    let doc = web.Load(url)
    let nodes = doc.DocumentNode.SelectNodes("//a[@href]")
    let rec forEachNode (nodes : HtmlNodeCollection) =
        match (nodes.Elements) with
        | [] -> ignore
        | head :: tail ->
            let client = WebClient()
            printf "%A" (client.DownloadData(head.Attributes["href"].Value))
            forEachNode tail
    forEachNode nodes
    