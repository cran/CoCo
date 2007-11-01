"ehFit" <-
function (model = FALSE, a = FALSE, b = FALSE, dual = NULL, sub.class = FALSE, 
    p.accepted = FALSE, data = NULL, object = .object.of.model(model, 
        data = data, ...), ...) 
{
    ".fit.EH" <- function(dual = "smallest.dual", sub.class = FALSE, 
        object = CoCoCore::.currentCoCo()) {
        coco.simple.command(225, sum(c(.encode(c("r.dual", "a.dual", 
            "smallest.dual", "largest.dual", "both.duals", "both.dual", 
            "both"), dual, c(1, 2, 3, 4, 5, 5, 5), dual), .encode(c("decomposable", 
            "graphical", "hierarchical"), sub.class, c(10, 20, 
            30), 0))), object = object)
    }
    if (p.accepted) 
        .set.acceptance(p.accepted, object = object)
    if ((model == "models") & is.gc(a)) 
        coco.enter.string(217, a, FALSE, object = object)
    else if ((length(dual) != 0)) 
        .fit.EH(dual, sub.class, object = object)
    else if ((model == "r.dual") | (model == "a.dual") | (model == 
        "smallest.dual") | (model == "largest.dual") | (model == 
        "both.duals") | (model == "both.dual") | (model == "both")) 
        .fit.EH(dual = model, sub.class = ifelse(sub.class, sub.class, 
            a), object = object)
    else if ((model == "base") | (model == "current") | (model == 
        "last") | (model == "all") | (model == "interval") | 
        (model == "list") | is.number(model) | is.vector(model)) 
        .to.search("fit", model, a, b, object = object)
    else if (is.gc(model)) 
        coco.enter.string(217, model, FALSE, object = object)
    else .to.search("fit", model, a, b, object = object)
}
