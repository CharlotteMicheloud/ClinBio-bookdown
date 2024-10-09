# Define variables
BOOK_FILE := _book/index.html  # Path to the generated HTML file
BROWSER := google-chrome       # Change this to your preferred browser

# Build the Bookdown project
build:
	Rscript -e "rmarkdown::render_site(output_format = 'bookdown::gitbook', encoding = 'UTF-8')"

# Open the HTML file
open:
	@echo "Opening the book in $(BROWSER)..."
	$(BROWSER) --new-window $(BOOK_FILE) > /dev/null 2>&1 &  # Run in the background


# Refresh the book by closing and reopening the Firefox window
refresh:
	@echo "Closing existing tab showing $(BOOK_FILE)..."; \
	if wmctrl -c "Clinical Biostatistics"; then \
		echo "Closed existing tab."; \
	else \
		echo "No existing tab to close."; \
	fi; \
	$(BROWSER) $(BOOK_FILE) > /dev/null 2>&1 &  # Run in the background
	
all: build refresh	
