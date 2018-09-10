RMDFILE=time_series_output

html :
	Rscript -e "require(knitr); require(markdown); knit('$(RMDFILE).Rmd', '$(RMDFILE).md'); markdownToHTML('$(RMDFILE).md', '$(RMDFILE).html', options=c('use_xhtml', 'base64_images')); browseURL(paste('file://', file.path(getwd(),'$(RMDFILE).html'), sep=''))"
