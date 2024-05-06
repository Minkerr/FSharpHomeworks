module Homework7.MiniCrawler

open System.Collections.Generic
open System.Net
open System.Text.RegularExpressions
open HtmlAgilityPack

//<a href="http://...">

let crawler (url : string)  = 
    let web = HtmlWeb()
    let doc = web.Load(url)
    let nodeCollection = doc.DocumentNode.SelectNodes("//a[@href]")
    let parseNodes (nodes : IEnumerable<HtmlNode>) =
        for node in nodes do
            let link = node.Attributes["href"]
            //printf $"\n%A{link.Value}\n"
            if link <> null then
                try 
                    let client = WebClient()
                    let res = client.DownloadString(url + link.Value)
                    printf "%d\n" res.Length
                with
                | ex -> ignore ex
    parseNodes nodeCollection
    
    