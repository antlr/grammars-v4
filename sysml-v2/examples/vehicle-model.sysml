package VehicleModel {
    /**
    * Basic vehicle model demonstrating SysML v2.0 syntax
    * including parts, attributes, ports, constraints, and requirements
    */

    part def Vehicle {
        part engine : Engine;
        part transmission : Transmission;
        part wheels : Wheel[4];
        port powerOut : PowerPort;

        attribute mass : Real;
        attribute maxSpeed : Real;

        constraint massConstraint {
            mass < 2000.0
        }

        constraint speedConstraint {
            maxSpeed > 60.0
        }
    }

    part def Engine {
        attribute power : Real;
        attribute mass : Real;
        attribute displacement : Real;
        port powerPort : PowerPort;

        constraint powerMassRatio {
            power / mass > 0.1
        }
    }

    part def Transmission :> Subsystem {
        attribute gearRatio : Real;
        attribute efficiency : Real;

        constraint efficiencyConstraint {
            efficiency >= 0.85 and efficiency <= 1.0
        }
    }

    part def Wheel {
        attribute diameter : Real;
        attribute width : Real;

        constraint sizeConstraint {
            diameter >= 14.0 and diameter <= 22.0
        }
    }

    part def Subsystem {
        attribute mass : Real;
        attribute cost : Real;
    }

    port def PowerPort {
        attribute voltage : Real;
        attribute current : Real;

        constraint powerLimit {
            voltage * current <= 1000.0
        }
    }

    requirement def PerformanceReq {
        doc /* The vehicle shall accelerate from 0 to 60 mph in less than 6 seconds */

        subject vehicle : Vehicle;

        require constraint {
            vehicle.engine.power >= 300.0
        }
    }

    requirement def SafetyReq {
        doc /* The vehicle shall have adequate braking performance */

        subject vehicle : Vehicle;

        require constraint {
            vehicle.mass / vehicle.wheels.size <= 500.0
        }
    }

    use case def DriveCar {
        actor driver : Driver;
        subject vehicle : Vehicle;

        objective {
            doc /* Successfully operate the vehicle from point A to point B */
        }
    }

    part def Driver {
        attribute experience : Real;
        attribute licenseValid : Boolean;
    }

    // Example vehicle instances
    part myCar : Vehicle {
        :>> engine {
            :>> power = 250.0;
            :>> mass = 150.0;
            :>> displacement = 2.0;
        }

        :>> transmission {
            :>> gearRatio = 3.5;
            :>> efficiency = 0.92;
        }

        :>> mass = 1500.0;
        :>> maxSpeed = 120.0;
    }
}
