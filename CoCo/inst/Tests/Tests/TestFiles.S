
### TestFiles

### 1) Missing "\0" at "what"
### 2) 70 -> 51
### 3) No effect of set?

  library(CoCo);
  library(CoCoOldData);
  ReinisCoCoObject <- Reinis()

  showOptions("files")

  str(optionsCoCo(section="files"))

  "coco.source" ;
  source.coco("what")

  "set.diary.file" ;
  sinkCoCo(type = "diary", file.name = "what")
  sinkCoCo(type = "diary", file.name = "Diary-1.tmp")
  sinkCoCo(type = "diary", file.name = "what")

  "set.output" ;
  set.output("what")

  "set.report.file" ;
  sinkCoCo(type = "report", file.name = "what")
  sinkCoCo(type = "report", file.name = "Report-1.tmp")
  sinkCoCo(type = "report", file.name = "what")

  "set.log.file" ;
  sinkCoCo(type = "log", file.name = "what")
  sinkCoCo(type = "log", file.name = "Log-1.tmp")
  sinkCoCo(type = "log", file.name = "what")

  "set.dump.file" ;
  sinkCoCo(type = "dump", file.name = "what")
  sinkCoCo(type = "dump", file.name = "Dump-1.tmp")
  sinkCoCo(type = "dump", file.name = "what")

  showOptions("files")

  FileOptions <- optionsCoCo(section="files")
  str(FileOptions)

  sinkCoCo(type = "diary",  file.name = "Diary-2.tmp")
  sinkCoCo(type = "report", file.name = "Report-2.tmp")
  sinkCoCo(type = "log",    file.name = "Log-2.tmp")
  sinkCoCo(type = "dump",   file.name = "Dump-2.tmp")

  str(optionsCoCo(section="files"))

  optionsCoCo(diary.name  = "Diary-3.tmp", force.files = TRUE)
  optionsCoCo(report.name = "Report-3.tmp", force.files = TRUE)
  optionsCoCo(log.name    = "Log-3.tmp", force.files = TRUE)
  optionsCoCo(dump.name   = "Dump-3.tmp", force.files = TRUE)

  str(optionsCoCo(section="files"))

  FileOptions <- optionsCoCo(FileOptions, section="files")
  str(FileOptions)

  str(optionsCoCo(section="files"))

  quitCoCo()
