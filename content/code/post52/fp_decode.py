def create_group_interpreter(range_start, range_end, status_map, group_name):
    def interpreter(ID, status):
        if range_start <= ID < range_end:
            device_nr = ID - range_start
            status_string = status_map.get(status, None)
            if status_string is None:
                return None
            else:
                return (f"{group_name} #{device_nr}: {status_string}")
        return None
    return interpreter


motor_status_map = {
    0: "Not moving",
    1: "Moving",
    2: "Hardware defect",
}

sensor_status_map = {
    0: "Ok",
    1: "Above threshold",
    2: "Below threshold",
}

gpio_status_map = {
    0: "Off",
    1: "On",
}


motor_interpreter = create_group_interpreter(20, 30, motor_status_map, "Motor")
sensor_interpreter = create_group_interpreter(40, 50, sensor_status_map, "Sensor")
gpio_interpreter = create_group_interpreter(50, 58, gpio_status_map, "GPIO")


def device_a_interpreter(ID, status):
    if ID == 2:
        status_map = {
            0: "Idle",
            1: "Processing",
            2: "Sending",
        }
        status_string = status_map.get(status, None)
        if status_string is None:
            return None
        return (f"Device_A: {status_string}")
    return None


def device_b_interpreter(ID, status):
    if ID == 4:
        status_map = {
            0: "Idle",
            1: "Calculating",
        }
        status_string = status_map.get(status, None)
        if status_string is None:
            return None
        return (f"Device_B: {status_string}")
    return None

# Main application code starts here

interpreters = [motor_interpreter,
                sensor_interpreter,
                gpio_interpreter,
                device_a_interpreter,
                device_b_interpreter]


def decode(ID, status):
    for interpreter in interpreters:
        interpretation = interpreter(ID, status)
        if interpretation is not None:
            return interpretation
    return None


if __name__ == '__main__':
    import sys
    import re

    message_re = re.compile(r'\((?P<ID>\d+),(?P<status>\d+)\)')
    protocol_dump_file = sys.argv[1]

    def unknown(ID, status):
        print(f"Unknown ID {ID} with status {status}")

    with open(protocol_dump_file, 'r') as protocol_dump:
        for line in protocol_dump.readlines():
            m = message_re.match(line)
            if m:
                ID = int(m.group('ID'))
                status = int(m.group('status'))
                decode_result = decode(ID, status)
                if decode_result is not None:
                    print(decode_result)
                else:
                    unknown(ID, status)
