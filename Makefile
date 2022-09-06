restart:
	R -e "rstudioapi::restartSession('shiny::runApp(\"main.r\")')"
	
style:
	R -e "styler::style_dir()"

sass:
	sass www/styles.scss www/styles.css

lazy:
	git add --all
	git commit -m 'save'
	git push



