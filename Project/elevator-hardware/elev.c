

#include "elev.h"

#include "channels.h"
#include "io.h"

#include <assert.h>
#include <stdlib.h>

#define MOTOR_SPEED 2800


static const int lamp_channel_matrix[N_FLOORS][N_BUTTONS] = {
    {LIGHT_UP1, LIGHT_DOWN1, LIGHT_COMMAND1},
    {LIGHT_UP2, LIGHT_DOWN2, LIGHT_COMMAND2},
    {LIGHT_UP3, LIGHT_DOWN3, LIGHT_COMMAND3},
    {LIGHT_UP4, LIGHT_DOWN4, LIGHT_COMMAND4},
};


static const int button_channel_matrix[N_FLOORS][N_BUTTONS] = {
    {BUTTON_UP1, BUTTON_DOWN1, BUTTON_COMMAND1},
    {BUTTON_UP2, BUTTON_DOWN2, BUTTON_COMMAND2},
    {BUTTON_UP3, BUTTON_DOWN3, BUTTON_COMMAND3},
    {BUTTON_UP4, BUTTON_DOWN4, BUTTON_COMMAND4},
};



void elev_init(void) {
    int init_success = io_init();
    assert(init_success && "Unable to initialize elevator hardware!");

    for (int f = 0; f < N_FLOORS; f++) {
        for (elev_button_type_t b = 0; b < N_BUTTONS; b++){
            elev_set_button_lamp(b, f, 0);
        }
    }

    elev_set_stop_lamp(0);
    elev_set_door_open_lamp(0);
    elev_set_floor_indicator(0);
}


void elev_set_motor_direction(elev_motor_direction_t dirn) {
    if (dirn == 0){
        io_write_analog(MOTOR, 0);
    } else if (dirn > 0) {
        io_clear_bit(MOTORDIR);
        io_write_analog(MOTOR, MOTOR_SPEED);
    } else if (dirn < 0) {
        io_set_bit(MOTORDIR);
        io_write_analog(MOTOR, MOTOR_SPEED);
    }
}


void elev_set_button_lamp(elev_button_type_t button, int floor, int value) {
    assert(floor >= 0);
    assert(floor < N_FLOORS);
    assert(button >= 0);
    assert(button < N_BUTTONS);

    if (value) {
        io_set_bit(lamp_channel_matrix[floor][button]);
    } else {
        io_clear_bit(lamp_channel_matrix[floor][button]);
    }
}


void elev_set_floor_indicator(int floor) {
    assert(floor >= 0);
    assert(floor < N_FLOORS);

    // Binary encoding. One light must always be on.
    if (floor & 0x02) {
        io_set_bit(LIGHT_FLOOR_IND1);
    } else {
        io_clear_bit(LIGHT_FLOOR_IND1);
    }    

    if (floor & 0x01) {
        io_set_bit(LIGHT_FLOOR_IND2);
    } else {
        io_clear_bit(LIGHT_FLOOR_IND2);
    }    
}


void elev_set_door_open_lamp(int value) {
    if (value) {
        io_set_bit(LIGHT_DOOR_OPEN);
    } else {
        io_clear_bit(LIGHT_DOOR_OPEN);
    }
}


void elev_set_stop_lamp(int value) {
    if (value) {
        io_set_bit(LIGHT_STOP);
    } else {
        io_clear_bit(LIGHT_STOP);
    }
}



int elev_get_button_signal(elev_button_type_t button, int floor) {
    assert(floor >= 0);
    assert(floor < N_FLOORS);
    assert(button >= 0);
    assert(button < N_BUTTONS);


    return io_read_bit(button_channel_matrix[floor][button]);
}


int elev_get_floor_sensor_signal(void) {
    if (io_read_bit(SENSOR_FLOOR1)) {
        return 0;
    } else if (io_read_bit(SENSOR_FLOOR2)) {
        return 1;
    } else if (io_read_bit(SENSOR_FLOOR3)) {
        return 2;
    } else if (io_read_bit(SENSOR_FLOOR4)) {
        return 3;
    } else {
        return -1;
    }
}


int elev_get_stop_signal(void) {
    return io_read_bit(STOP);
}


int elev_get_obstruction_signal(void) {
    return io_read_bit(OBSTRUCTION);
}





