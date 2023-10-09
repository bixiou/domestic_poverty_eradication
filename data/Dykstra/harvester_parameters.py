#encoding=UTF-8

# Benjamin Dykstra
# bendykst@gmail.com

# Written for Python 3.3

# Gathers parameters for Lorenz curves
# from all Povcal surveys

# At the time of writing, this scrips requires
# Firefox 22, due to Selenium's browser.execute_script() 
# not working in newer versions of the browser.

import os
import re
from html.parser import HTMLParser
from selenium import webdriver

#
# Utility Classes
#

class TagRemover(HTMLParser):
    # Removes HTML tags from a string
    def __init__(self):
        super().__init__()
        self.reset()
        self.fed = []
    def handle_data(self, d):
        self.fed.append(d)
    def get_data(self):
        return ''.join(self.fed)

class TableParser(HTMLParser):
    # Given an HTML table body, returns cell data as
    # a list of rows each containing a list of cells
    # E.g. [ row_0_cell_0, row_0_cell_1, ...], [row_1_cell_0, ...]]
    def __init__(self):
        super().__init__()
        self.cell_buffer = "" # Incomplete data from the current cell
        self.cells = []       # Completed cells in the current row
        self.rows = []        # Completed rows in the current table
        self.in_cell = False
    def handle_starttag(self, tag, attrs):
        # When entering a cell, set the in_cell flag
        # and start recording data.
        if tag == 'td':
            self.in_cell = True
    def handle_endtag(self, tag):
        if tag == 'tr':
            # When leaving a row, if cells have been found,
            # add the list of cells to the list of rows and
            # and clear the list of cells.
            if len(self.cells):
                self.rows.append(self.cells)
                self.cells = []
        elif tag == 'td':
            # Else if leaving a cell, unset the cell flag so
            # that non-cell data will be ignored, add the
            # data buffer as a new cell and clear the buffer.
            self.in_cell = False
            self.cells.append(self.cell_buffer)
            self.cell_buffer = ""
    def handle_data(self, data):
        # Are we in a cell? If so, record the data.
        if self.in_cell:
            self.cell_buffer += data

#
# Utility functions
#

def format_text(text):
    # Removes external whitespace and all asterisks from a string.
    text = text.strip()
    text = text.replace('*', '')
    return text

def strip_tags(html):
    # Removes HTML tags from a string
    s = TagRemover()
    s.feed(html)
    return s.get_data()

def find_lines(lines, pattern, offset=0, count=1):
    # Searches for lines that match pattern and
    # returns {count} consecutive lines starting at
    # {offset} offset from each match
    #
    # E.g. for count=2, returns [["Match 1 Line 1", Match 1 Line 2"], ["Match 2 Line 1", ...], ...]

    matching_line_numbers = [idx for idx, line in enumerate(lines) if re.match(pattern, line)]
    lines = [lines[line + offset: line + offset + count] for line in matching_line_numbers]

    return lines

#
# Selenium Functions
#
# Each function in this group accepts 'browser' as its
# first argument, a Selenium Firefox WebDriver object.
#

def select_countries(browser, cached_countries, queries):
    # Selects a list of countries on the first page of PovcalNet
    #
    #   cached_countries: A list of all Povcal countries in Povcal order
    #   queries: A list of of dicts containing the countries to be selected
    #            [{"country": "Albania", ...}, ...]

    # Wait until the element is found.
    browser.find_element_by_id("CountryName")

    # First, the country selection box identified. Then a list
    # of options is selected using their index positions.
    # Important! Povcal lists countries in an order that is not strictly
    # alphabetical. A cached list must be used to determine the correct index values.
    js = "sele = document.getElementById('CountryName'); "
    template = "sele.options[{}].selected = true; "
    for query in queries:
        idx = cached_countries.index(query["country"])
        js += template.format(idx)
    js += "return true;"
    browser.execute_script(js)
    return

def parse_results(browser):
    # Parses the result table
    # Returns a list of dicts. E.g. [{"country": "Mexico",
    #                                 "year": "1995",
    #                                 "type": "C",
    #                                 "poverty_line": "1,200.10",
    #                                 "headcount": "95.12"}, ... ]

    table_body = browser.find_element_by_xpath('//body//table//tbody//tr[2]//td//div//p//table//tbody')
    table_text = table_body.get_attribute("innerHTML")
    table_parser = TableParser()
    # Use the table parser to split the string into rows of cells.
    # E.g. [ row_0_cell_0, row_0_cell_1, ...], [row_1_cell_0, ...]]
    table_parser.feed(table_text)

    results = []
    for row in table_parser.rows:
        result = {}
        result["country"]      = format_text(row[0])
        result["year"]         = format_text(row[1])
        result["type"]         = format_text(row[2])
        result["poverty_line"] = format_text(row[3])
        result["headcount"]    = format_text(row[5])
        results.append(result)
    return results

#
# Setup
#

cached_countries = []

browser = webdriver.Firefox()
browser.maximize_window()
browser.implicitly_wait(24)

# Cache the list of countries for quick searching and index lookup
browser.get("http://iresearch.worldbank.org/PovcalNet/index.htm?2") # Load page
browser.switch_to_frame("contentBody") # Switch into the context of the embedded frame
country_selection_box = browser.find_element_by_id("CountryName") #Find the country multiselect box
all_options = country_selection_box.find_elements_by_tag_name("option") #Get a list of all options
for option in all_options:  # Iterate over all options and cache them
    country_name = format_text(option.get_attribute("text"))
    cached_countries.append(country_name)

# Initialize the output files
working_dir = os.getcwd()
output_path = "{}/{}".format(working_dir, "output")
if not os.path.exists(output_path):
    os.mkdir(output_path)
data_file = "{0}/curve_parameters.csv".format(output_path)
error_file = "{0}/failed_queries.txt".format(output_path)

# Initialize the list of columns and the csv
cols = ["country", "year", "type", "a", "b", "c", "theta", "gamma", "delta", "best_curve", "curve_validity"]
with open(data_file, 'w') as fout:
    fout.write("| ".join(cols))
    fout.write("\n")

with open(error_file, 'w') as fout:
    fout.write("Failed queries\n")

#
# Main loop
#

for country in cached_countries:
    # For each country in Povcal's list, this loop navigates to the results page
    # using default values for all forms and selecting all report years.
    # Then, it pulls up the 'detail output' page for each year, finding the
    # relevant information and recording it in pipe-delimited format. (Table
    # data will contain commas.) If the desired information is not found,
    # it records the failed attempt in a separate file.

    select_countries(browser, cached_countries, [{"country": country}])
    browser.find_element_by_name("AddSelected").click() # Add the selected countries to the "Your selection" box
    browser.find_element_by_name("SubmitCountries").click() # Click the 'Continue' button
    browser.find_element_by_id("chkAllYear").click() # Select all years
    browser.find_element_by_name("SubmitValue").click() #Click the submit button
    browser.switch_to_frame("ifrResultFrame") # Switch into the context of the embedded frame
    result_table = parse_results(browser)

    for row_num, row in enumerate(result_table):
        # For each row in the result table,
        # process the detail output

        # Record the current window to return to later
        root_window = browser.window_handles[0]

        # SRF(n) requests the report for row n
        js = "SRF({}); return true;".format(row_num)
        browser.execute_script(js)

        # There should be two windows open now. Open the one that isn't the root window.
        popup_window = next(item for item in browser.window_handles if item != root_window)
        browser.switch_to_window(popup_window)

        # Get the text from the report and then switch back to the previous window and iframe
        data = browser.find_element_by_id("txtResult").get_attribute("innerHTML")
        browser.switch_to_window(root_window)
        browser.switch_to_frame("contentBody")
        browser.switch_to_frame("ifrResultFrame")

        # Split the text into a list of lines. Then find the desired lines using
        # nearby lines with known values
        lines = [str.strip() for str in data.split('\n')]
        quad_matches = find_lines(lines, r"THE PARAMETERS OF GENERAL QUADRATIC LORENZ CURV", offset=2)
        implied_matches = find_lines(lines, r"THE IMPLIED PARAMETERS OF BETA LORENZ CURVE ARE", offset=2)
        sentence_matches = find_lines(lines, r"\*\*\*\*\* Final result \*\*\*\*\*", count=2, offset=-2)

        # If we found all the lines, then record the data. Otherwise create
        # an entry in the error file.
        if all([quad_matches, implied_matches, sentence_matches]):
            # Parse the lines and store them in a dict
            gen_quad_params = [float(n) for n in quad_matches[0][0].split()]
            implied_params = [float(n) for n in implied_matches[0][0].split()]
            sentences = sentence_matches[0]

            p_a, p_b, p_c = gen_quad_params
            p_theta, p_gamma, p_delta = implied_params
            best_curve, curve_validity = sentences
            gathered_data = {
                "country": row["country"],
                "year": row["year"],
                "type": row["type"],
                "a": p_a,
                "b": p_b,
                "c": p_c,
                "theta": p_theta,
                "gamma": p_gamma,
                "delta": p_delta,
                "best_curve": best_curve,
                "curve_validity": curve_validity}

            # Store the dict in a csv, using cols as a reference for column position
            row_text = "| ".join([str(gathered_data[col]) for col in cols])
            with open(data_file, 'a') as fout:
                fout.write(row_text)
                fout.write("\n")
        else:
            with open(error_file, 'a') as fout:
                fout.write("{}| {}| {}\n".format(row["country"], row["year"], row["type"]))

    # Switch back to the main page for the next country
    browser.get("http://iresearch.worldbank.org/PovcalNet/index.htm?2") # Load page
    browser.switch_to_frame("contentBody") # Switch into the context of the embedded frame
print("Good day")
