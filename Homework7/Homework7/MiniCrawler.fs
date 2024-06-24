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
                let page = web.Load(link.Value).DocumentNode.InnerText
                if link <> null && link.Value.Substring(0, 8) = "https://" then
                    let result =
                        try 
                            let result = page.Length
                            (link.Value, Some result)
                        with
                        | e -> (link.Value, None)
                    return result
                else return (link.Value, None)
            }
        )
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Seq.toList 
    parseNodes nodeCollection
    
    