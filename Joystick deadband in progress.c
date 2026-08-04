/* COMMAND 12 - SET JOYSTICK BASE REGISTERS TO CURRENT VALUES OF JOYSTICK READINGS,
AND SET JOYSTICK DEADBAND. */

set_adc_base:
	jnb	numok,noprm	
	mov	dband,cparam
	mov	dband+1,cparam+1
noprm:	call	setbase
	mov	a,#1
	ret
{
    Joystick leftStick;
    Joystick rightStick;
    RobotDrive drive;
 
     void(); 
        leftStick(1),   //Please use correct Joystick port
        rightStick(2),  //Please use correct Joystick port
        drive(1, 2)     //Please use correct PWM channels
    {
    }
	
    void OperatorControl()
    {
        while (IsEnable() && IsOperatorControl())
        {
            float leftPower = DEADBAND_INPUT(-leftStick.GetY());
            float rightPower = DEADBAND_INPUT(-rightStick.GetY());
            drive.TankDrive(leftPower, rightPower);
            Wait(0.05);
        }
    }
};