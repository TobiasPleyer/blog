# Let filename be the right path
with open('post38_example.csv', 'r') as fp:
    header = fp.readline()
    lines = fp.readlines()
rows = [(float(x[0]), int(x[1]), int(x[2]), int(x[3])) for x in [line.split(',') for line in lines]]
# Remove those rows with invalid data
valid_rows = [row for row in rows if row[3]==1]
# Extract the timestamps from the measurement series
times = [x[0] for x in valid_rows]
# Calculate the time differences to find the starts and ends of packages
time_diffs = enumerate([x[1]-x[0] for x in zip(times[:-1], times[1:])])
# Find those indices where the time differnce is greater than the inhibit time
package_idxs = [0] + [x[0]+1 for x in time_diffs if x[1]>1e-4]
# Zip those bounds into (start, end) pairs
package_bounds = list(zip(package_idxs[:-1], package_idxs[1:]))
# Just take every other bounds pair (alternating data constraint)
data1_bounds = package_bounds[::2]
data2_bounds = package_bounds[1::2]
# Finally extract the slices and only take the data row entries
data1_packages = [[a[1] for a in z] for z in [y for y in [valid_rows[x[0]:x[1]] for x in data1_bounds]]]
data2_packages = [[a[2] for a in z] for z in [y for y in [valid_rows[x[0]:x[1]] for x in data2_bounds]]]
