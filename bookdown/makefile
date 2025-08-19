# Define variables
BOOK_FILE := _book/index.html  # Path to the generated HTML file
BROWSER := chromium       # Change this to your preferred browser

# Build the Bookdown project
build:
	Rscript -e "rmarkdown::render_site(output_format = 'bookdown::gitbook', encoding = 'UTF-8')"

# Open the HTML file
open:
	@echo "Opening the book in $(BROWSER)..."
	$(BROWSER) --new-window $(BOOK_FILE) > /dev/null 2>&1 &  # Run in the background


# Refresh the book by closing and reopening the Chrome window
refresh:
	@echo "Closing existing tab showing $(BOOK_FILE)..."; \
	if wmctrl -c "Clinical Biostatistics"; then \
		echo "Closed existing tab."; \
	else \
		echo "No existing tab to close."; \
	fi; \
	$(BROWSER) $(BOOK_FILE) > /dev/null 2>&1 &  # Run in the background


publishL:
	Rscript -e "rsconnect::deploySite(appMode = 'static', server = 'bookdown.org', appId = '14572', account = 'held_leonhard_uzh')"

publishC:
	Rscript -e "rsconnect::deploySite(appMode = 'static', server = 'bookdown.org', appId = '14572', account = 'charlotte_micheloud93')"

publishS:
	Rscript -e "rsconnect::deploySite(appMode = 'static', server = 'bookdown.org', appId = '14572', account = 'stefanie_vonfelten')"

all: build refresh	
