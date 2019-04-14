customer_hash = { "John Doe": {"first": "John", "last": "Doe", "age": 42} }
customer_array = 123456*[{}] + [{"first": "John", "last": "Doe", "age": 42}]

def lookupByName(s):
    return customer_hash[s]

def lookupByIndex(i):
    return customer_array[i]

def lookup(key):
    typ = type(key)
    if typ == str:
        return lookupByString(key)
    elif typ == int:
        return lookupByIndex(key)
    else:
        raise Exception("Bad argument")

print(lookup("John Doe"))
print(lookup(123456))
