
### TestFiles

### 1) Missing "\0" at "what"
### 2) 70 -> 51
### 3) No effect of set?

  library(CoCo);
  library(CoCoOldData);
  ReinisCoCoObject <- Reinis()

  status("files")

  str(optionsCoCo(section="files"))

  "coco.source" ;
  source.coco("what")

  "set.diary.file" ;
  set.output.file(file = "diary", file.name = "what")
  set.output.file(file = "diary", file.name = "Diary-1.tmp")
  set.output.file(file = "diary", file.name = "what")

  "set.output" ;
  set.output("what")

  "set.report.file" ;
  set.output.file(file = "report", file.name = "what")
  set.output.file(file = "report", file.name = "Report-1.tmp")
  set.output.file(file = "report", file.name = "what")

  "set.log.file" ;
  set.output.file(file = "log", file.name = "what")
  set.output.file(file = "log", file.name = "Log-1.tmp")
  set.output.file(file = "log", file.name = "what")

  "set.dump.file" ;
  set.output.file(file = "dump", file.name = "what")
  set.output.file(file = "dump", file.name = "Dump-1.tmp")
  set.output.file(file = "dump", file.name = "what")

  status("files")

  FileOptions <- optionsCoCo(section="files")
  str(FileOptions)

  set.output.file(file = "diary",  file.name = "Diary-2.tmp")
  set.output.file(file = "report", file.name = "Report-2.tmp")
  set.output.file(file = "log",    file.name = "Log-2.tmp")
  set.output.file(file = "dump",   file.name = "Dump-2.tmp")

  str(optionsCoCo(section="files"))

  optionsCoCo(diary.name  = "Diary-3.tmp", force.files = TRUE)
  optionsCoCo(report.name = "Report-3.tmp", force.files = TRUE)
  optionsCoCo(log.name    = "Log-3.tmp", force.files = TRUE)
  optionsCoCo(dump.name   = "Dump-3.tmp", force.files = TRUE)

  str(optionsCoCo(section="files"))

  FileOptions <- optionsCoCo(FileOptions, section="files")
  str(FileOptions)

  str(optionsCoCo(section="files"))

  .quit()
