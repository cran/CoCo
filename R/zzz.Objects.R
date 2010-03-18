    setClass("CoCoIdClass", representation(.reference = "numeric", 
        .id.env = "character", .key = "character", .type = "numeric", 
        .title = "character"))
    setClass("CoCoClass", representation("CoCoIdClass", .parameters = "list", 
        .invalid = "list", .specification = "list", .medio = "list", 
        .observations = "list", .structure = "list"))
    setClass("CoCoModelClass", representation("CoCoIdClass", 
        .model = "character", .model.number = "numeric"))
