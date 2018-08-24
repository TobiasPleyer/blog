def create_motor_interpreter(motor_id, motor_name):
    motor_network_id = motor_id + 20
    status_map = {
        0: "Not moving",
        1: "Moving",
        2: "Hardware defect",
    }
    def interpreter(ID, status):
        if ID == motor_network_id:
            status_string = status_map[status]
            return (f"Motor {motor_name}: {status_string}")
        return None
    return interpreter


def create_sensor_interpreter(sensor_id, sensor_name):
    sensor_network_id = sensor_id + 40
    status_map = {
        0: "Ok",
        1: "Above threshold",
        2: "Below threshold",
    }
    def interpreter(ID, status):
        if ID == sensor_network_id:
            status_string = status_map[status]
            return (f"Sensor {sensor_name}: {status_string}")
        return None
    return interpreter


def create_gpio_interpreter(gpio_id, gpio_name):
    gpio_network_id = gpio_id + 50
    status_map = {
        0: "Off",
        1: "On",
    }
    def interpreter(ID, status):
        if ID == gpio_network_id:
            status_string = status_map[status]
            return (f"GPIO {gpio_name}: {status_string}")
        return None
    return interpreter


def device_a_interpreter(ID, status):
    status_map = {
        0: "Idle",
        1: "Processing",
        2: "Sending",
    }
    if ID == 2:
        status_string = status_map[status]
        return (f"Device_A: {status_string}")
    return None


def device_b_interpreter(ID, status):
    status_map = {
        0: "Idle",
        1: "Calculating",
    }
    if ID == 4:
        status_string = status_map[status]
        return (f"Device_B: {status_string}")
    return None


motors = [
    (1, "Motor_A"),
    (2, "Motor_B"),
    (3, "Motor_C"),
]


sensors = [
    (1, "Sensor_A"),
    (2, "Sensor_B"),
]


gpios = [
    (1, "GPIO_A"),
    (2, "GPIO_B"),
]


interpreters = [create_motor_interpreter(motor_id, motor_name)
                for motor_id, motor_name in motors]
interpreters += [create_sensor_interpreter(sensor_id, sensor_name)
                 for sensor_id, sensor_name in sensors]
interpreters += [create_gpio_interpreter(gpio_id, gpio_name)
                 for gpio_id, gpio_name in gpios]
interpreters += [device_a_interpreter,
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
