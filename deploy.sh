# copy files to shiny server dir
git pull

cp index.Rmd /srv/shiny-server/taxed/
cp tax_engine.R /srv/shiny-server/taxed/
cp eic.feather /srv/shiny-server/taxed/
cp marriage-cost.feather /srv/shiny-server/taxed/
cp google-analytics.js /srv/shiny-server/taxed/