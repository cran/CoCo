"forward" <-
function (sorted = FALSE, reversed = FALSE, only = FALSE, short = FALSE, 
    p.accepted = FALSE, p.rejected = FALSE, decomposable.mode = NULL, 
    coherent = FALSE, headlong = FALSE, recursive = FALSE, all.significant = TRUE, 
    components = NULL, p.components = FALSE, separators = FALSE, 
    p.separators = FALSE, edges = TRUE, model = FALSE, fix.edges = NULL, 
    data = NULL, object = .object.of.model(model, data = data, 
        ...), ...) 
{
    old.current <- .before.set.current(model, object = object)
    if (!(is.null(fix.edges))) 
        .fix.edges(fix.edges, object = object)
    if (p.components) 
        .set.components(p.components, object = object)
    if (p.separators) 
        .set.separators(p.separators, object = object)
    if (p.accepted) 
        .set.acceptance(p.accepted, object = object)
    if (p.rejected) 
        .set.rejection(p.rejected, object = object)
    if (!is.null(decomposable.mode)) 
        .set.switch("decomposable.mode", ifelse(decomposable.mode, 
            "on", "off"), object = object)
    if (!is.null(components)) 
        .set.switch("partitioning", ifelse(decomposable.mode, 
            "on", "off"), object = object)
    if (only) 
        coco.simple.command(199, 1, object = object)
    if (reversed) 
        coco.simple.command(199, 2, object = object)
    if (sorted) 
        coco.simple.command(199, 3, object = object)
    if (short) 
        coco.simple.command(199, 5, object = object)
    if (recursive) 
        coco.simple.command(199, 10, object = object)
    if (coherent) 
        coco.simple.command(199, 11, object = object)
    if (headlong) 
        coco.simple.command(199, 12, object = object)
    if (separators) 
        coco.simple.command(199, 15, object = object)
    if (all.significant != TRUE) 
        coco.simple.command(199, 20, object = object)
    coco.simple.command(201, .encode(c("edges", "interactions"), 
        edges, c(1, 2), ifelse(edges, 1, 2)), object = object)
    if (old.current) 
        makeCurrent(old.current, object = object)
}
