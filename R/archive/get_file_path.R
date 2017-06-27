frame_files <- Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))
thisfile <- file.path(getwd(), frame_files[[length(frame_files)]])
print(thisfile)
# PATH <- dirname(frame_files[[length(frame_files)]])
