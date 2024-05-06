module Homework7.MiniCrawler

open System.Collections.Generic
open HtmlAgilityPack

let crawler (url : string)  = 
    let web = HtmlWeb()
    let doc = web.Load(url)
    let nodeCollection = doc.DocumentNode.SelectNodes("//a[@href]")
    let parseNodes (nodes : IEnumerable<HtmlNode>)=
        nodes
        |> Seq.map (fun node ->
            async{
                let link = node.Attributes["href"]
                if link <> null && link.Value.Substring(0, 8) = "https://" then
                    let result =
                        try 
                            web.Load(link.Value).DocumentNode.InnerText.Length
                        with
                        | e -> -1
                    return result
                else return -1
            }
        )
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Seq.toList 
    parseNodes nodeCollection
    
    