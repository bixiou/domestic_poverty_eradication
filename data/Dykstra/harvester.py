#encoding=UTF-8

# Benjamin Dykstra 
# bendykst@gmail.com

# Written for Python 3.3

# Gathers income and consumption data from PovcalNet 
# in a series of increasingly detailed passes.
# Given enough time, will gather all data.

import time
import json
import pdb
from itertools import tee
from collections import defaultdict
from html.parser import HTMLParser
from selenium import webdriver
from selenium.webdriver.remote.command import Command
from pymongo import MongoClient, ASCENDING, DESCENDING

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

class QueryScheduler():
    # On PovcalNet, you can make more than one query at a time. However, 
    # queries involving the same country must share a poverty line.    
    # The QueryScheduler class takes a list of queries as input
    # and combines them into blocks. To balance the need for efficiency 
    # and the desire to respect PovcalNet's servers, blocks must not be too 
    # large nor too small. (~100 queries is good.)     
    
    def __init__(self, batch_size, buffer_length):
        # Instance variables:
        #     _blocks[]: Each block in blocks is a list of queries that can
        #                be executed concurrently.
        #
        #     _ready: The query scheduler is "ready" to provide a block if either:
        #               * There is a block containing at least batch_size queries.
        #               * There are at least buffer_length blocks.
        #
        #     _batch_size: The target number of queries to make at a time.        
        #
        #     _buffer_length: In some cases, blocks accumulate without any single
        #                     block reaching the desired size. When the number of
        #                     blocks is equal to buffer_length, ready() reports true
        #                     even if all blocks are too small.
        
        self._blocks = []
        self._ready = False
        self._batch_size = batch_size
        self._buffer_length = buffer_length
    
    def batch(self, queries):
        # Take a list of queries as input 
        # and add each to a block, provided that:
        #  a. The block is not too large
        #  b. The block does not contain a 
        #     query with the same country.
        # If no suitable block is found, create a new one.
        # 
        # queries: Of the form, {"country": country, "years": ["1995.5", "2000", ...]}
        
        for query in queries:
            for block in reversed(self._blocks):
                # "If the block is smaller than the maximum size and there is no query in it with the same country as this query."
                if query_len(block) < self._batch_size and not next((item for item in block if item["country"] == query["country"]), None):
                    block.append(query)
                    # If a block has reached the desired size, then the QueryScheduler is "ready".
                    if query_len(block) >= self._batch_size: 
                        self._ready = True
                    break
            else: 
                # No suitable blocks were found, so create a new one.
                self._blocks.append([query]) 
                if len(self._blocks) >= self._buffer_length:
                    self._ready = True
        # Sort the blocks so the largest will be removed first
        self._blocks.sort(key=lambda block: query_len(block)) 

    def ready_blocks(self):
        # Yields a block of queries while the query scheduler is 
        # "ready". For the definition of ready, see above.
        while self.is_ready():
            next_queries = self._blocks.pop()
            # Check if the scheduler is still in a ready 
            # state after popping the block. Are there blocks?
            # Are any large enough? (They are sorted according
            # to size, so we need only check the largest one.)
            # Are there too many?
            if len(self._blocks) == 0: 
                self._ready = False
            elif query_len(self._blocks[-1]) < self._batch_size: 
                if len(self._blocks) < self._buffer_length:
                    self._ready = False
                else:
                    self._ready = True
            else:
                self._ready = True
            yield next_queries
    
    def all_blocks(self):
        # This is used to flush the block buffer at the end
        # of a pass. Decisions about queries in the next pass 
        # will be based on the results of this pass, so it is
        # important that they all be completed.
        while not self.is_empty():
            yield self._blocks.pop()    

    def is_ready(self): 
        # Is the query scheduler "ready" to provide a block?
        # For the definition of ready, see above.
        return self._ready

    def is_empty(self):
        # True if there are no blocks.
        return (len(self._blocks) < 1)

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

def float_range(start, stop, step):    
    # Yields an arithmetic series of floats
    # from (start+step) to stop, inclusive.
    # (0, 1, 4) => 1, 2, 3, 4
    r = start + step
    while r <= stop:
        yield r
        r += step 

def pairwise(iterable):
    # Takes a list and returns a second list of the first's successive pairs.
    # E.g. [a, b, c, d] -> [(a, b), (b, c), (c, d)]
    a, b = tee(iterable)
    next(b, None)
    return zip(a, b)

last_query_time = time.time()
def wait_patiently(delay):
    # Wait until 'delay' seconds have passed since last query
    # Along with query scheduler, this is used to avoid negatively
    # affecting PovcalNet.
    global last_query_time
    while time.time() - last_query_time < delay:
        time.sleep(0.1)
    last_query_time = time.time()
    return

def query_len(queries):
    # Returns the total size of a query block.
    # E.g. [{"country": "Zambia", "years": [1999, 2001, 2003]}] has a 
    # size of three.
    length = 0
    for query in queries:
        length += len(query["years"])
    return length

def commit_results(db, queries, results):
    # Commit headcount results to the database
    #  
    # Queries: The list of queries used to produce the results. A great
    #          majority of the time, this is not needed, but in some cases,
    #          PovcalNet returns a row of n/a values. The list is used to
    #          determine which query fails.
    #
    # Results: The results taken from the HTML table. Cell text has been 
    #          formatted to remove HTML tags, external whitespace and asterisks.

    for result in results:
        # Convert the headcount value to integer cents. If NaN, use 'na' instead.
        try:
            headcount = float(result["headcount"]) 
            result["headcount"] = int(round(headcount * 100))
        except:
            result["headcount"] = "na" 

        # Remove the poverty line string from the collection,
        # and remove any commas from it. E.g. "1,993" => "1993"
        pl_str = result.pop("poverty_line", None) 
        pl_str = pl_str.replace(",", "")

        # Convert the poverty line string to integer cents, 
        # store the results in the database and remove the query
        # from the intention journal.
        try:
            result["pl_cents"] = int(round(float(pl_str) * 100)) 
            db.headcounts.insert(result)
            db.journal.remove({"country": result["country"], "pl_str": pl_str}) 
        except Exception as e:
            # If the above fails, it is very inconvenient; PovcalNet has probably returned a row of n/a for all values except 
            # country and year. This means that we can't use the table to determine the poverty line or whether the survey looks 
            # at consumption or income (hereafter referred to as 'type'.) 
            # 
            # The first thing that we do is build a list of surveys from all known surveys that match the country and the year. This list 
            # might contain one item, e.g. "Brazil 1999 Consumption", or two items, "Brazil 1999 Consumption" and  "Brazil 1999 Income".
            # Next, we compare each of these potential surveys to the entire result table. If a row in the result table is found to match
            # all three country, year and type, then we know that that query succeeded and it cannot be the query that we are trying to repair. 
            # However, if it is not found, then we have discovered the missing information. Using the list of queries, we recover the poverty line 
            # string. Using the matching surveys, we recover the survey type. Then we add a flag so that we know the query failed and insert 
            # the document into the database.
            if pl_str == 'n/a':
                query = next((item for item in queries if item["country"] == result["country"]), None)
                if query:
                    # Find surveys in the database that match the missing row
                    matching_surveys = [item for item in db.surveys.find({"country": result["country"], "year": result["year"]})]
                    surveyCompare = lambda x, y: x["country"] == y["country"] and x["year"] == y["year"] and x["type"] == y["type"] 
                    # For each possible match, look to see if it exists in the result table.
                    # If it does, it can't be the missing row.
                    for matching_survey in matching_surveys:
                        matching_results = (item for item in results if surveyCompare(item, matching_survey))
                        if not next(matching_results, None):
                            result["type"] = matching_survey["type"]

                    pl_str = query["pl_str"]
                    result["pl_cents"] = int(round(float(pl_str) * 100)) 
                    result["headcount"] = "na"
                    result["bad_pl_returned"] = True 
                    db.headcounts.insert(result)
                    db.journal.remove({"country": result["country"], "pl_str": pl_str}) 
    return

def check_interval(survey, midpoint):    
    # Determine if a survey's headcount changes over a 
    # poverty line interval. If it doesn't change, then
    # we can ignore the interval as headcount should be
    # monotonically non-decreasing.
    #
    # Args:
    #    survey: A survey. E.g. {"country": "Brazil", "type": "C": "year", "1999"}
    #    midpoint: The the midpoint of the interval in cents
    # Returns: True or False

    country = survey["country"]
    year = survey["year"]
    data_type = survey["type"]

    # Find the adjacent queries, in terms of poverty line
    preceeding_row = db.headcounts.find({"country": country, "year": year, "type": data_type, "pl_cents" : {"$lt" : midpoint}}, {"_id": 0}).sort("pl_cents", DESCENDING).limit(1)[0]
    following_row  = db.headcounts.find({"country": country, "year": year, "type": data_type, "pl_cents" : {"$gt" : midpoint}}, {"_id": 0}).sort("pl_cents", ASCENDING ).limit(1)[0]

    # If the headcounts are the same,then we know that all headcounts in the middle will also be the same.
    # Update: This assumption turns out not to be true in all cases. However, the fluctuations are so small
    #         (+/- 0.01%) and the time savings are so great that we have stuck with this optimization.
    if preceeding_row["headcount"] == following_row["headcount"]: 
        return False
    else:
        return True

def process_journal(browser, db, cached_countries):
    # Sometimes, between the time a query is added to the 
    # scheduler and when the results are committed to the database,
    # the process is interrupted. This might be due to an error or 
    # because this script was manually restarted. In such cases, the query 
    # will be preserved in the intention journal in the database. 
    # The journal is processed immediately whenever the script is restarted 
    # and after each pass over the income spectrum. If any query fails 
    # a second time, the script is halted.
    
    # Create a temporary scheduler and fetch queries from the journal
    query_scheduler = QueryScheduler(100, 20)
    journal_length = db.journal.find({}, {"_id": 0}).count()
    missed_queries = db.journal.find({}, {"_id": 0}) 
    queries = [item for item in missed_queries]    

    if journal_length != 0:
        print("Processing journaled queries...")    
    
    # Add all journaled queries to the scheduler and execute them all. 
    query_scheduler.batch(queries) 
    schedule = query_scheduler.all_blocks()
    for queries in schedule:
        try:
            results = query_many(browser, cached_countries, queries)
            commit_results(db, queries, results)
        except Exception as e:
            print("Error executing a block of queries: {}.".format(str(e)))
            print("This was the second attempt and the script will quit to protect the data.")
            print("Here is the list of queries:")
            print(queries)    
            quit()
    if journal_length != 0:
        print("Journal processed successfully. Continuing normally.")

#
# Selenium Functions
#
# Each function in this group accepts 'browser' as its 
# first argument, a Selenium Firefox WebDriver object.
#

def select_option(browser, element_id, option_text):
    # Given the element id of a select box and the 
    # value of an option present within, select that option
   
    # Selenium will wait until the element is present
    browser.find_element_by_id(element_id) 
                                          
    # Selenium is very slow at traversing the DOM. 
    # To quickly select the option, we inject a 
    # javacript function into the iframe.
    js = """sele = document.getElementById('{0}');
    for (var i = 0; i < sele.options.length; i++) {{
        if(sele.options[i].value === '{1}') sele.options[i].selected = true;
    }} return true;""".format(element_id, option_text)
    browser.execute_script(js)
       
def select_options(browser, selects):
    # Given a list of select box ids and
    # desired options, selects those options
    #
    # selects: [{"select_id": "an_id", "select_options": ["option1", "option2", ...]}, ...]

    browser.find_element_by_id(selects[0]["select_id"]) # This will cause Selenium to wait until
                                                        # the element is found.

    # Selenium is very slow at traversing the DOM. 
    # To quickly select the options, we inject a 
    # javacript function into the iframe. The collection
    # of element ids and option ids is serialized 
    # as a Javascript object literal using the json module.
    selects = json.dumps(selects)
    js = "var selects = {};".format(selects)
    js += """
    for (var k = 0; k < selects.length; k++) {
        var options = selects[k]["select_options"];
        var sele = document.getElementById(selects[k]["select_id"]);
        for (var j = 0; j < options.length; j++) {
            option = options[j];
            for (var i = 0; i < sele.options.length; i++) {
                if(sele.options[i].value === option) sele.options[i].selected = true;
            }
        }
    }
    return true;"""
    browser.execute_script(js)

def input_text(browser, inputs):
    # Fills a list of text boxes
    #
    # inputs: [{"input_id": "someId", "input_str": "some string"}, ... ]

    # This will cause Selenium to wait until the element is found.
    browser.find_element_by_id(inputs[0]["input_id"]) 

    # Selenium is very slow at traversing the DOM. 
    # To quickly input text in many boxes, we inject a 
    # javacript function into the iframe. The collection
    # of textbox ids and strings is serialized 
    # as a Javascript object literal using the json module.
    inputs= json.dumps(inputs)
    js = "var inputs = {};".format(inputs)
    js += """
    console.log(inputs)
    for (var k = 0; k < inputs.length; k++) {
        var inputStr = inputs[k]["input_str"];
        var input = document.getElementById(inputs[k]["input_id"]);
        input.value = inputStr;
    }
    return true;"""
    browser.execute_script(js)

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
 
    browser.switch_to_frame("ifrResultFrame") # Switch into the context of the embedded frame
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

def query_many(browser, cached_countries, queries):
    # Submits a list of queries to PovcalNet
    # 
    # cached_countries: A list of all Povcal countries in Povcal order
    # queries: A list of queries, each a dict containing the country name,
    #          the poverty line as a string and a list of years.
    #          {"country": "Albania", "pl_str": "13.12", "years": [1992, 1996, 2004]}
    
    # PovcalNet sorts country names in idiosyncratic order (alphabetically first, then by
    # length in descending order, e.g. [a, ba, b, c].) Cached_countries preserves this order.
    # The following line sorts the queries according to Povcal logic using cached_countries
    # as a reference. After being sorted, each query's index in the list will be the same 
    # as the indices of its corresponding text and option boxes on a PovcalNet page.
    queries.sort(key=lambda x: cached_countries.index(x["country"]))

    browser.get("http://iresearch.worldbank.org/PovcalNet/index.htm?2") # Load start page
    browser.switch_to_frame("contentBody") # Switch into the context of the embedded frame
    select_countries(browser, cached_countries, queries) # Select all countries in queries   
    browser.find_element_by_name("AddSelected").click() # Add the selected countries to the "Your selection" box
    browser.find_element_by_name("SubmitCountries").click() # Click the 'Continue' button
    
    # For each country, year and poverty line, controls are named SELE{n} and PL{n}, where
    # n is the country's position in an ordered subset of cached_countries.
    for idx, query in enumerate(queries):
        query["select_id"] = "SELE{}".format(idx)
        query["input_id"]  =   "PL{}".format(idx)
        query["select_options"] = query["years"]
        query["input_str"]      = query["pl_str"]
    select_options(browser, queries)
    input_text(browser, queries)
    
    wait_patiently(3) # Wait until three seconds since the last query.

    browser.find_element_by_name("SubmitValue").click() # Click the 'submit' button
    results = parse_results(browser) # Parse the results
    return results 

def query_all(browser, pl_str):
    # Queries all countries and all years for the same poverty line
    # pl_str is the poverty line as a string. E.g. "1532.12"
    browser.get("http://iresearch.worldbank.org/PovcalNet/index.htm?2") # Load page
    browser.switch_to_frame("contentBody") # Switch into the context of the embedded frame
    browser.find_element_by_name("AddAll").click() # Add the selected country to the "Your selection" box
    
    browser.execute(Command.ACCEPT_ALERT) # Clear the "Are you sure?" modal alert
    browser.find_element_by_name("SubmitCountries").click() # Click the 'Continue' button
    browser.find_element_by_id("chkAllYear").click() # Select all years

    PL0_text_box = browser.find_element_by_name("PL0")
    PL0_text_box.clear() 
    PL0_text_box.send_keys( pl_str ) # Input the PL
    browser.find_element_by_id("btnSetToAll").click() # Set the same PL for all countries
    
    browser.find_element_by_name("SubmitValue").click() #Click the submit button
    results = parse_results(browser) #Parse the result table
    return results

#
# Setup
#    

# Connect to MongoDb                 
mongo = MongoClient()
db = mongo.povcal

# Create indices for fast searching
db.headcounts.ensure_index([("country", ASCENDING), ("year", ASCENDING), ("pl_cents", ASCENDING), ("type", ASCENDING)]) 
db.queries.ensure_index([("pl_cents", ASCENDING)])
db.countries.ensure_index([("name", ASCENDING)], unique=True, drop_dups=True)
db.journal.ensure_index([("country", ASCENDING), ("pl_cents", ASCENDING), ("type", ASCENDING)])

cached_countries = [] # Preloaded country names for quick searching and index lookup
query_scheduler = QueryScheduler(100, 20) # Create a query scheduler with 100 item queries

browser = webdriver.Firefox() # Create a session of Firefox
browser.maximize_window()
browser.implicitly_wait(30) # Configure the WebDriver to wait up to 30 seconds for each page to load

# Cache the list of countries for quick searching and index lookup
browser.get("http://iresearch.worldbank.org/PovcalNet/index.htm?2") # Load page
browser.switch_to_frame("contentBody") # Switch into the context of the embedded frame
country_selection_box = browser.find_element_by_id("CountryName") #Find the country multiselect box
all_options = country_selection_box.find_elements_by_tag_name("option") #Get a list of all options
for option in all_options:  # Iterate over all options and cache them
    country_name = format_text( option.get_attribute("text") )    
    cached_countries.append(country_name)
    try:
        db.countries.insert({"name": country_name}) # Add the country to a small collection
    except:
        pass #Quietly drop duplicates

# Do an initial set of queries for all countries using a poverty line of 1c.
previous_query = db.queries.find_one({"pl_cents": 1}) # Check if this has already been done to save time. 
if not previous_query:
    results = query_all(browser, "0.01") #Query at 1c
    queries = [] # The queries argument is normally used for error recovery.
                 # However, the type of error for which a list of queries is 
                 # necessary does not occur at 1c. It is safe to pass an empty list.
    commit_results(db, queries, results)
    for result in results:
        survey_record = {"country": result["country"],
                         "year":    result["year"],
                         "type":    result["type"]}
        db.surveys.insert(survey_record) # Create a list of surveys
    db.queries.insert({"pl_cents": 1}) # Record the fact that we have completed the initial set of queries
                        
# Cache the list of surveys
# These are in the form: {"country": "Albania", "year": "1988", "type": "C"}    
surveys = [item for item in db.surveys.find({}, {"_id": 0})]

# Look in the journal for any queries that were not completed 
# during previous runs and complete them.
process_journal(browser, db, cached_countries)

#
# First pass 
#                 
# Make a first pass over four areas
#  Area 1: 0.1 to 100 in 0.1 increments
#  Area 2: 101 to 1000 in 1 increments
#  Area 3: 1010 to 10000 in 10 increments
#  Area 4: 11,000 to 100,000 in 1000 increments
search_areas = [(0, 100, 0.1),
                (100, 1000, 1),
                (1000, 10000, 10),
                (10000, 100000, 1000)]
for search_area in search_areas:
    area_iter = float_range(*search_area)
    for poverty_line in area_iter:
        # Convert the pl to integer cents and a string.
        pl_cents = int(round(poverty_line * 100)) 
        pl_str = "{0:.2f}".format(poverty_line)   

        # Make sure we're not performing a query that has already been done. If so, skip it.
        prior_query = db.queries.find_one({"pl_cents": pl_cents}) 
        if prior_query:
            continue
       
        # Create a dict of queries, organized by country: {"Albania": [1999, 2001, 2008], ...}
        # Convert the dict into a list of tuples: [("Albania", [1992, 1999, 2002]), ("Cambodia", [2000, 2003], ...]
        queries = defaultdict(list)
        for survey in surveys:
            queries[survey["country"]].append(survey["year"])
        queries = list(queries.items()) 
                                       

        # For each survey in each tuple, generate a query for the current 
        # poverty line and a corresponding entry for the intention journal.
        query_collection = []
        journal_entries = []
        for country, years in queries:
            query_collection.append({"country": country, "years": years, "pl_str": pl_str})
            journal_entries.append({"country": country, "years": years, "pl_str": pl_str})

        # If there is at least one query, add the journal entries to the journal
        # and add the queries to the query scheduler. Then, while the query scheduler
        # has blocks ready to go, execute those blocks.
        if len(journal_entries) > 0:
            db.journal.insert(journal_entries) 
        db.queries.insert({"pl_cents": pl_cents}) 
        query_scheduler.batch(query_collection) 
        schedule = query_scheduler.ready_blocks()
        for queries in schedule:
            results = query_many(browser, cached_countries, queries)
            commit_results(db, queries, results)
            
# The first pass is complete, but there might be some 
# blocks of queries still resident in the scheduler. 
# The scheduler needs to be emptied before the next pass.
schedule = query_scheduler.all_blocks()
for queries in schedule: 
    results = query_many(browser, cached_countries, queries)
    commit_results(db, queries, results)
            
# There might have been errors. Check the journal to 
# see if any queries failed. Execute any that were found.
process_journal(browser, db, cached_countries)

#
# Remaining passes 
#
work_is_finished = False # Loops until no queries are made
while work_is_finished == False:
    work_is_finished = True

    # Loop over each successive pair of previous queries whose poverty lines are separated by more than one cent.
    previous_queries = db.queries.find({}, {"_id": 0}, timeout=False).sort("pl_cents", ASCENDING) 
    for low, high in pairwise(previous_queries): 
        if high["pl_cents"] - low["pl_cents"] <= 1: 
            continue
        
        # Find the midpoint between previous queries and round to the nearest cent
        mid = int(round((low["pl_cents"] + high["pl_cents"])/2)) 
        pl_str = "{0:.2f}".format(mid/100) 
        print("Planning pl {}".format(pl_str)) 

        # For the current interval (e.g. $100 to $101) and each 
        # survey (country, year, type), check if a query needs to be made.
        # If it does, add it to a dict of queries, organized by country:
        # {"Albania": [1999, 2001, 2008], ...}
        queries = defaultdict(list)
        for survey in surveys:
            if check_interval(survey, mid):
                queries[survey["country"]].append(survey["year"])
        # Convert the dict into a list of tuples: [("Albania", [1992, 1999, 2002]), ("Cambodia", [2000, 2003], ...]
        queries = list(queries.items()) 

        # For each survey in each tuple, generate a query for the current poverty line
        # and a corresponding entry for the intention journal.
        query_collection = []
        journal_entries = []
        for country, years in queries:
            query_collection.append({"country": country, "years": years, "pl_str": pl_str})
            journal_entries.append({"country": country, "years": years, "pl_str": pl_str})

        # If there is at least one query, add the journal entries to the journal
        # and add the queries to the query scheduler. Then, while the query scheduler
        # has blocks ready to go, execute those blocks.
        if len(journal_entries) > 0:
            db.journal.insert(journal_entries) 
        db.queries.insert({"pl_cents": mid}) 
        query_scheduler.batch(query_collection) 
        schedule = query_scheduler.ready_blocks()
        for queries in schedule:
            print("Executing {} queries...".format(query_len(queries)))
            work_is_finished = False
            try:
                results = query_many(browser, cached_countries, queries) 
                commit_results(db, queries, results)
            except Exception as e:
                print("Error executing a block of queries: {}.".format(str(e)))
                print("The block will be attempted again later.")
        
    # The pass is complete, but there might be some 
    # blocks of queries still resident in the scheduler. 
    # The scheduler needs to be emptied before the next pass.
    schedule = query_scheduler.all_blocks()
    print("\nPass complete. Flushing task queue.")
    for queries in schedule: # Flush the scheduler
        print("Executing {} queries...".format(query_len(queries)))
        work_is_finished = False
        try:
            results = query_many(browser, cached_countries, queries)
            commit_results(db, queries, results)
        except Exception as e:
            print("Error executing a block of queries: {}.".format(str(e)))
            print("The block will be attempted again later.")
    print("\n")

    # There might have been errors. Check the journal to 
    # see if any queries failed. Execute any that were found.
    process_journal(browser, db, cached_countries)    

browser.close()
print("Good day")
