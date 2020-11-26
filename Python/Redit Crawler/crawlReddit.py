import pandas as pd
import requests
import json
import csv
import time
import datetime

def getPushshiftData(after, before, sub):
    url = 'https://api.pushshift.io/reddit/search/submission/?size=1000&after='+str(after)+'&before='+str(before)+'&subreddit='+str(sub)
    print(url)
    r = requests.get(url)
    data = json.loads(r.text)
    return data['data']


def collectSubData(subm):
    subData = list()  # list to store data points
    title = subm['title']
    url = subm['url']
    try:
        flair = subm['link_flair_text']
    except KeyError:
        flair = "NaN"
    author = subm['author']
    sub_id = subm['id']
    score = subm['score']
    created = datetime.datetime.fromtimestamp(subm['created_utc'])  # 1520561700.0
    numComms = subm['num_comments']
    permalink = subm['permalink']
    try:
        selftext = subm['selftext']
    except KeyError:
        selftext = "NaN"

    subData.append((sub_id, title, url, author, score, created, numComms, permalink, flair, selftext))
    subStats[sub_id] = subData

#Subreddit to query
sub='brexit'
#before and after dates
#before = "1554449460"
#after = "1466640000"

before = "1554451161"
after = "1443657600"


subCount = 0
subStats = {}

data = getPushshiftData(after, before, sub)
# Will run until all posts have been gathered
# from the 'after' date up until before date
while len(data) > 0:
    for submission in data:
        collectSubData(submission)
        subCount += 1
    # Calls getPushshiftData() with the created date of the last submission
    print(len(data))
    print(str(datetime.datetime.fromtimestamp(data[-1]['created_utc'])))
    after = data[-1]['created_utc']
    data = getPushshiftData(after, before, sub)

print(len(data))

print(str(len(subStats)) + " submissions have added to list")
print("1st entry is:")
print(list(subStats.values())[0][0][1] + " created: " + str(list(subStats.values())[0][0][5]))
print("Last entry is:")
print(list(subStats.values())[-1][0][1] + " created: " + str(list(subStats.values())[-1][0][5]))


def updateSubs_file():
    upload_count = 0
    file = "diffusions_submissions_extra.csv"
    with open(file, 'w', newline='', encoding='utf-8') as file:
        a = csv.writer(file, delimiter=',')
        headers = ["Post ID", "Title", "Url", "Author", "Score", "Publish Date", "Total No. of Comments", "Permalink",
                   "Flair", "Selftext"]
        a.writerow(headers)
        for sub in subStats:
            a.writerow(subStats[sub][0])
            upload_count += 1

        print(str(upload_count) + " submissions have been uploaded")


updateSubs_file()

#############################################################################

def getComments(after, before, sub):
    url = 'https://api.pushshift.io/reddit/search/comment/?size=1000&after='+str(after)+'&before='+str(before)+'&subreddit='+str(sub)
    print(url)
    r = requests.get(url)
    data = json.loads(r.text)
    return data['data']


def collectCommData(comm):
    commData = list()  # list to store data points
    author = comm['author']
    body = comm['body']
    comm_id = comm['id']
    score = comm['score']
    created = datetime.datetime.fromtimestamp(comm['created_utc'])  # 1520561700.0
    try:
        permalink = comm['permalink']
    except KeyError:
        permalink = "NaN"

    submId = comm['link_id']
    parentId = comm['parent_id']

    commData.append((comm_id, author, body, score, created, permalink, submId, parentId))
    commStats[comm_id] = commData


#before = "1554449460"
#after = "1466640000"

before = "1554451161"
after = "1443657600"

commCount = 0
commStats = {}

data = getComments(after, before, sub)
# Will run until all comments have been gathered
# from the 'after' date up until before date
while len(data) > 0:
    for comment in data:
        collectCommData(comment)
        commCount += 1

    print(len(data))
    print(str(datetime.datetime.fromtimestamp(data[-1]['created_utc'])))
    after = data[-1]['created_utc']
    data = getComments(after, before, sub)

print(len(data))

def updateComms_file():
    upload_count = 0
    file = "diffusions_comments_extra.csv"
    with open(file, 'w', newline='', encoding='utf-8') as file:
        a = csv.writer(file, delimiter=',')
        headers = ["Comment ID", "Author", "Body", "Score", "Publish Date", "Permalink",
                   "Submission ID", "Parent ID"]
        a.writerow(headers)
        for comm in commStats:
            a.writerow(commStats[comm][0])
            upload_count += 1

        print(str(upload_count) + " Comments have been uploaded")


updateComms_file()