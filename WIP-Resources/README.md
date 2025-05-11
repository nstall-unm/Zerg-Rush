# WIP Resources

This is a file full of assets that are currently being worked on. This serves as backup of files that might be used will be used, or as a storage for editable versions of files (i.e. aseprite files)

## NOTE

Please note that if messing with these files any transparent sections must match the background as bmp does not support transparency. Also If anyone is messing with .aseprite files know that Brillo does not like how aseprite exports .bmp files and will not compile. I was able to get around this issue by running mogrify -format bmp -define bmp:format=bmp3 resources/"folder that you need"/*.bmp