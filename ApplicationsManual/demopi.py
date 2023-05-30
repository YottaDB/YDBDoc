#################################################################
#                                                               #
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.  #
# All rights reserved.                                          #
#                                                               #
#   This source code contains the intellectual property         #
#   of its copyright holder(s), and is made available           #
#   under a license.  If you do not know the terms of           #
#   the license, please stop and do not read further.           #
#                                                               #
#################################################################

from subprocess import Popen, PIPE
from time import sleep, ctime
from datetime import datetime
import board
import digitalio
import busio
import adafruit_sgp30
import adafruit_character_lcd.character_lcd as characterlcd
import yottadb
import glob

# Modify this if you have a different sized character LCD
lcd_columns = 16
lcd_rows = 2

# compatible with all versions of RPI as of Jan. 2019
# v1 - v3B+
lcd_rs = digitalio.DigitalInOut(board.D22)
lcd_en = digitalio.DigitalInOut(board.D17)
lcd_d4 = digitalio.DigitalInOut(board.D25)
lcd_d5 = digitalio.DigitalInOut(board.D24)
lcd_d6 = digitalio.DigitalInOut(board.D23)
lcd_d7 = digitalio.DigitalInOut(board.D18)

# Initialise the lcd class
lcd = characterlcd.Character_LCD_Mono(lcd_rs, lcd_en, lcd_d4, lcd_d5, lcd_d6,
                                      lcd_d7, lcd_columns, lcd_rows)

# pir at BCM port 20
# PIR sensitivity (detect range) can be tuned by adjusting the screw on the right side of the buzzer
# led at BCM port 12
pir_gpio = 20
led_gpio = 12

GPIO.setmode(GPIO.BCM)
GPIO.setup(pir_gpio, GPIO.IN)
GPIO.setup(led_gpio, GPIO.OUT, initial=GPIO.LOW) # Led off by default

temp_base_dir = glob.glob('/sys/bus/w1/devices/' + '28*')[0]
temp_file     = temp_base_dir + '/temperature'

# run unix shell command, return as ASCII
def run_cmd(cmd):
    p = Popen(cmd, shell=True, stdout=PIPE)
    output = p.communicate()[0]
    return output.decode('ascii')

# wipe LCD screen before we start
lcd.clear()

while True:

    # date and time
    lcd_line_1 = datetime.now().strftime('%b %d  %H:%M:%S\n')

    # Read temperature
    f = open(temp_file, 'r')
    raw_temp = f.readline().strip()
    if raw_temp == '': continue

    temp_unrounded = int(raw_temp)/1000
    f.close()
    temp_rounded = round(temp_unrounded,1)

    # Read motion sensor
    pir_out = 0
    if(GPIO.input(pir_gpio) == 0):
        pir_out=0
        GPIO.output(led_gpio, GPIO.LOW)
    elif(GPIO.input(pir_gpio) == 1):
        pir_out=1
        GPIO.output(led_gpio, GPIO.HIGH)

    # Read CO2, and TV0C
    try:
        i2c_bus = busio.I2C(board.SCL, board.SDA, frequency=100000)
        sgp30 = adafruit_sgp30.Adafruit_SGP30(i2c_bus)
        eCO2, TVOC = sgp30.iaq_measure()
    except:
        eCO2, TVOC = 0, 0

    lcd_line_2 = str(pir_out) + '|' + str(temp_rounded) + '|' + str(eCO2) + '|' + str(TVOC) + '       '
    yottadb.Key('^motion')[datetime.now().strftime("%m-%d-%Y %H:%M:%S")].value = str(pir_out) + '|' + str(temp_rounded) + '|' + str(eCO2) + '|' + str(TVOC)

    # combine both lines into one update to the display
    lcd.message = lcd_line_1 + lcd_line_2

sleep(1)
