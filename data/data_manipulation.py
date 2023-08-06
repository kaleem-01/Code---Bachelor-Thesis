# Data Manipulation for Bachelor thesis
# Author: Kaleem Ullah

import pandas as pd

# Open file and create a dataframe with successes and failures marked as 1 and 0
df = pd.read_csv("data.csv")


# Lists with starting position and sequences
start = [i for i in df["start"]]
sequence = [i for i in df["sequence"]]

# Sequences without 'x' and 'y'
clean_sequence = []
for each_trial in sequence:
    clean_string = ''
    for each_string in each_trial:
        if each_string in ['h', 't']:
            clean_string += each_string
    clean_sequence.append(clean_string)


# Creating data for both hypotheses

# Data for same-side hypothesis
trials_same = []
for i in range(len(sequence)):
    same_side = []
    previous = start[i]
    for each_string in clean_sequence[i]:
        if previous == each_string:
            same_side.append(1)
        else:
            same_side.append(0)
        previous = each_string
    trials_same.append(same_side)

# Data for head-tail hypothesis
trials_head = []
for i in range(len(clean_sequence)):
    heads = []
    for each_string in clean_sequence[i]:
        if each_string == 'h':
            heads.append(1)
        else:
            heads.append(0)
    trials_head.append(heads)

# Add outcome to dataframe
df['same_side'] = trials_same
df['head-tail'] = trials_head

# Creating dataframe with mapping with people
people = df['person'].unique()

same_list = []
heads_list = []
person = []
j = 1
for each_person in people:
    same_data = df['same_side'][df['person'] == each_person]
    heads_data = df['head-tail'][df['person'] == each_person]

    temp_same = [same_list.append(item) for sublist in same_data for item in sublist]
    temp_heads = [heads_list.append(item) for sublist in heads_data for item in sublist]
    [person.append(j) for i in range(len(temp_heads))]
    j += 1

# Creating dataframe with mapping with coins
coins = df['coin'].unique()

same_list = []
heads_list = []
coin = []
j = 1
for each_coin in coins:
    same_data = df['same_side'][df['coin'] == each_coin]
    heads_data = df['head-tail'][df['coin'] == each_coin]

    temp_same = [same_list.append(item) for sublist in same_data for item in sublist]
    temp_heads = [heads_list.append(item) for sublist in heads_data for item in sublist]
    [coin.append(j) for i in range(len(temp_heads))]
    j += 1



# Creating csv to migrate the data
all_data = pd.DataFrame()
all_data["same_side"] = same_list
all_data["heads"] = heads_list
all_data["person"] = person
all_data["coin"] = coin
all_data.to_csv("all_trials.csv")


