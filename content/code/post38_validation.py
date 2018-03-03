def cons(x, xs):
    return [x] + xs
# Let filename be the right path
with open('post38_example.csv', 'r') as fp:
    header = fp.readline()
    lines = fp.readlines()
rows = (list
         (map
           (lambda x: (float(x[0]), int(x[1]), int(x[2]), int(x[3])),
            (map
              (lambda x: x.split(','),
               lines)))))
valid_rows = (list
               (filter
                 (lambda x: x[3]==1,
                  rows)))
times = (list
          (map
            (lambda x: x[0],
             valid_rows)))
time_diffs = (list
               (enumerate
                 (map
                   (lambda x: x[1]-x[0],
                    zip(times[:-1], times[1:])))))
package_idxs = (cons
                 (0,
                  (list
                    (map
                      (lambda x: x[0]+1,
                       (filter
                         (lambda x: x[1] > 1e-4,
                          time_diffs)))))))
package_bounds = (list
                   (zip
                     (package_idxs[:-1],
                      package_idxs[1:])))
data1_bounds = package_bounds[::2]
data2_bounds = package_bounds[1::2]
data1_packages = (list
                   (map
                      (lambda x: list
                                   (map(lambda y: y[1], x)),
                       (map
                         (lambda z: valid_rows[z[0]:z[1]],
                          data1_bounds)))))
data2_packages = (list
                   (map
                      (lambda x: list
                                   (map(lambda y: y[2], x)),
                       (map
                         (lambda z: valid_rows[z[0]:z[1]],
                          data2_bounds)))))

print("Data1")
print(data1_packages)
print("Data2")
print(data2_packages)
