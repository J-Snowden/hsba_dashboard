library(shinylive)

# Export your app to "docs"
export(appdir = ".", destdir = "docs")

index_path <- file.path("docs", "index.html")
lines <- readLines(index_path)

# Update <title> tag (optional fallback)
title_line <- grep("<title>.*</title>", lines)
if (length(title_line)) {
  lines[title_line] <- "<title>HSBA Data Dashboard</title>"
}

# Add JS snippet to override title dynamically near end of body
body_end_line <- grep("</body>", lines)[1]
js_script <- '
<script>
  setTimeout(() => {
    document.title = "HSBA Data Dashboard";
  }, 500);
</script>'
lines <- append(lines, js_script, after = body_end_line - 1)

writeLines(lines, index_path)

cat("Exported app with dynamic browser tab title override (no icon).\n")
