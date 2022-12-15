namespace fsadvent

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating

[<JavaScript>]
module Client =
    type IndexTemplate = Template<"wwwroot/index.html", ClientLoad.FromDocument>

    let WIDTH = 600.
    let HEIGHT = 600.
    let animationHandle : Var<JS.Handle option> = Var.Create None

    type Dart =
        | Triple of int
        | Double of int
        | Single of int
        | Nil

        static member AsScore (d: Dart) =
            match d with
            | Triple v -> 3*v
            | Double v -> 2*v
            | Single v -> v
            | Nil -> 0

        static member AsDartNotation (d: Dart) =
            match d with
            | Triple v -> sprintf "T%d" v
            | Double v -> if v = 25 then "Bull" else sprintf "D%d" v
            | Single v -> sprintf "%d" v
            | Nil -> ""

        static member IsCheckout (d: Dart) =
            match d with
            | Double _ -> true
            | _ -> false
             
    type DartsSet = Dart * Dart * Dart

    let PrettyPrintDartsSet (d : DartsSet) =
        match d with
        | (d1, Nil, Nil) -> 
            sprintf "%s"
                (Dart.AsDartNotation d1)
        | (d1, d2, Nil) -> 
            sprintf "%s - %s"
                (Dart.AsDartNotation d1)
                (Dart.AsDartNotation d2)
        | (d1, d2, d3) -> 
            sprintf "%s - %s - %s"
                (Dart.AsDartNotation d1)
                (Dart.AsDartNotation d2)
                (Dart.AsDartNotation d3)

    let scoringValues =
        [
            for i in 1..20 do
                (Triple i)
                (Double i)
                (Single i)
            (Single 25)
            (Double 25)
            Nil
        ]
        |> List.groupBy Dart.AsScore
        |> Map.ofList

    // Calculate all combinations for a given number between 1 and 180

    let getCombinationsFor (numberToReach: int) (isItCheckout: bool) =
        scoringValues.Keys
        |> List.ofSeq
        |> List.choose (fun key ->
            if key = 0 && numberToReach <> 0 then
                None
            else if numberToReach - key < 0 then
                None
            else
                if isItCheckout && numberToReach - key = 0 && key % 2 = 0 then
                    Some (key, numberToReach - key)
                else if isItCheckout && numberToReach - key = 0 then
                    None
                else
                    Some (key, numberToReach - key)
        )
        |> List.collect (fun (v1, remainder) ->
            scoringValues.Keys
            |> List.ofSeq
            |> List.choose (fun key ->
                if key = 0 && remainder <> 0 then
                    None
                else if remainder - key < 0 then
                    None
                else
                    if remainder - key = 0 && isItCheckout && key % 2 = 0 then
                        Some (v1, key, remainder - key)
                    else if remainder - key = 0 && isItCheckout then
                        None
                    else
                        if scoringValues.ContainsKey (remainder - key) && (not isItCheckout || (remainder - key) % 2 = 0) then
                            Some (v1, key, remainder - key)
                        else
                            None
            )
        )
        |> List.collect (fun (d1, d2, d3) ->
            match scoringValues.TryGetValue(d1), scoringValues.TryGetValue(d2), scoringValues.TryGetValue(d3) with
            | (true, items1), (true, [Nil]), (true, [Nil]) ->
                if isItCheckout then
                    items1 |> List.filter Dart.IsCheckout |> List.map (fun x -> x, Nil, Nil)
                else
                    items1 |> List.map (fun x -> x, Nil, Nil)
            | (true, items1), (true, items2), (true, [Nil]) ->
                if isItCheckout then
                    items1 |> List.collect (fun x -> items2 |> List.filter Dart.IsCheckout |> List.map (fun y -> x, y, Nil))
                else
                    items1 |> List.collect (fun x -> items2 |> List.map (fun y -> x, y, Nil))
            | (true, items1), (true, items2), (true, items3) ->
                if isItCheckout then
                    items1 |> List.collect (fun x -> items2 |> List.collect (fun y -> items3 |> List.filter Dart.IsCheckout |> List.map (fun z -> x, y, z)))
                else
                    items1 |> List.collect (fun x -> items2 |> List.collect (fun y -> items3 |> List.map (fun z -> x, y, z)))
            | _ -> []
        )


    let resetBoard (ctx: CanvasRenderingContext2D) =
        ctx.ClearRect(0., 0., WIDTH, HEIGHT)

    let renderCircle (ctx: CanvasRenderingContext2D) radius color = 
        ctx.BeginPath();
        ctx.Arc(WIDTH/2., HEIGHT/2., radius, 0, 2. * Math.PI, false)
        ctx.LineWidth <- 1.
        ctx.StrokeStyle <- color
        ctx.FillStyle <- color
        ctx.Fill()
        ctx.Stroke()

    let convertToRadian (deg: int) =
        (float deg) * Math.PI / 180.

    let renderSector (ctx: CanvasRenderingContext2D) radius color (nth: int) = 
        let beginDeg = (-99 + nth * 18) // We are starting to render from the value of 20 clockwise
        let beginRad = convertToRadian beginDeg
        let endRad = convertToRadian (beginDeg + 18)
        ctx.BeginPath();
        ctx.MoveTo(WIDTH/2., HEIGHT/2.)
        ctx.Arc(WIDTH/2., HEIGHT/2., radius, beginRad, endRad, false)
        ctx.LineWidth <- 1.
        ctx.StrokeStyle <- color
        ctx.LineTo(WIDTH/2., HEIGHT/2.)
        ctx.FillStyle <- color
        ctx.Fill()
        ctx.Stroke()

    let dartRingNumbers =
        [
            20; 1; 18; 4; 13; 6; 10; 15; 2; 17; 3; 19; 7; 16; 8; 11; 14; 9; 12; 5
        ]

    let renderLabel (ctx: CanvasRenderingContext2D) (num: int) (nth: int) =
        ctx.Save()
        ctx.StrokeStyle <- "white"
        ctx.FillStyle <- "white"
        ctx.Font <- "30px Arial"
        ctx.TextAlign <- CanvasTextAlign.Center
        ctx.Translate(WIDTH/2., HEIGHT/2.)
        // The second quadrant should match the orientation of the fourth one
        if nth > 5 && nth < 10 then
            ctx.Rotate(convertToRadian ((20 + nth - 10) * 18))
        // 3 should match the orientation of 20
        else if nth = 10 then
            ctx.Rotate(convertToRadian 0)
        // The third quadrant should match the orientation of the first one
        else if nth > 10 && nth < 15 then
            ctx.Rotate(convertToRadian ((nth - 10) * 18))
        else
            ctx.Rotate(convertToRadian (nth * 18))
        //ctx.Rotate(convertToRadian (nth * 18))
        ctx.FillText(string num, 0, if nth > 5 && nth < 15 then HEIGHT/2. * 0.85 + 15. else - HEIGHT/2. * 0.85);
        ctx.Restore();
         
    let drawDartboard (ctx: CanvasRenderingContext2D) (dartToHighlight: Dart) =
        // Init board
        let hightlightColor = "#61bff9"
        resetBoard ctx
        let radius = WIDTH/2. - 10.
        let scoringRadius = (radius * 0.75)

        // Render outer circle
        renderCircle ctx radius "#11191f"

        for (index, num) in dartRingNumbers |> List.indexed do
            let doubleColor =
                match dartToHighlight with
                | Double v when num = v -> hightlightColor
                | _ -> if index % 2 = 0 then "#e63322" else "#389536" 
            let tripleColor =
                match dartToHighlight with
                | Triple v when num = v -> hightlightColor
                | _ -> if index % 2 = 0 then "#e63322" else "#389536" 
            let normalColor =
                match dartToHighlight with
                | Single v when num = v -> hightlightColor
                | _ -> if index % 2 = 0 then "#171918" else "#f7e0b6"
            // draw double point ring
            renderSector ctx (scoringRadius) doubleColor index
            // draw normal ring
            renderSector ctx (scoringRadius * 0.953) normalColor index
            // draw triple point ring
            renderSector ctx (scoringRadius * 0.694) tripleColor index
            // draw inner normal ring
            renderSector ctx (scoringRadius * 0.629) normalColor index
            // draw label
            renderLabel ctx num index

        let colorSmallBull = if dartToHighlight = Single 25 then hightlightColor else "#389536"
        let colorBigBull = if dartToHighlight = Double 25 then hightlightColor else "#e63322"
        // draw small bullseye point
        renderCircle ctx (scoringRadius * 0.094) colorSmallBull

        // draw bulls eye
        renderCircle ctx (scoringRadius * 0.037) colorBigBull

    let highlightDartboard (ctx: CanvasRenderingContext2D) (currentSelection: DartsSet option) =
        match currentSelection with
        | None ->
            drawDartboard ctx Nil
        | Some (d1, d2, d3) ->
            let mutable step = 1
            let highlightAnimation () =
                JS.Document.QuerySelector("span.highlighted") |> fun x -> if x !==. null then x.ClassList.Remove("highlighted")
                let dartToUse =
                    if step = 1 then
                        d1
                    else if step = 2 then
                        d2
                    else
                        d3

                drawDartboard ctx dartToUse
                // highlight matching result field
                JS.Document.GetElementById("dart" + string step).ClassList.Add("highlighted")
                if step = 3 then
                    step <- 1
                else
                    step <- step + 1

            animationHandle.Set (Some <| JS.SetInterval highlightAnimation 1000 )

    [<SPAEntryPoint>]
    let Main () =
        let selected = Var.Create None
        let numberToReach = Var.Create 1
        let isItCheckout = Var.Create false
        let results = Var.Create []
        IndexTemplate.Main()
            .CanvasAttr(
                [
                    on.viewUpdate selected.View (fun elem currentSelection ->
                        animationHandle.View
                        |> View.Get (fun handle ->
                            match handle with
                            | None -> ()
                            | Some handle ->
                                JS.ClearInterval handle
                                animationHandle.Set None
                        )
                        let canvasElement = As<CanvasElement> elem
                        let context = canvasElement.GetContext("2d")
                        highlightDartboard context currentSelection
                    )
                    on.afterRender (fun elem ->
                        let canvasElement = As<CanvasElement> elem
                        let context = canvasElement.GetContext("2d")
                        drawDartboard context Nil
                    )
                ]
            )
            .IsItCheckout(isItCheckout)
            .SelectedNumber(numberToReach)
            .Selected1(selected.View.Map(function None -> "" | Some (d1, _, _) -> Dart.AsDartNotation d1))
            .Selected2(selected.View.Map(function None -> "" | Some (_, d2, _) -> Dart.AsDartNotation d2))
            .Selected3(selected.View.Map(function None -> "" | Some (_, _, d3) -> Dart.AsDartNotation d3))
            .Calculate(fun te ->
                numberToReach.View
                |> View.Get (fun number ->
                    isItCheckout.View
                    |> View.Get (fun checkout ->
                        animationHandle.View
                        |> View.Get (fun handle ->
                            match handle with
                            | None -> ()
                            | Some handle ->
                                JS.ClearInterval handle
                                animationHandle.Set None
                        )
                        let res = getCombinationsFor number checkout
                        if res.Length = 0 then
                            JS.Alert <| sprintf "%d cannot be reached with 3 darts! Please try a different number." number
                        results.Set res
                        selected.Set None
                    )
                
                )
            )
            .SelectionBox(
                Doc.InputType.SelectDynOptional [] "None" PrettyPrintDartsSet results.View selected
            )
            .Doc()
        |> Doc.RunById "main"
