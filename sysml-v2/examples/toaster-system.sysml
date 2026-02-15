/**
* Simple Toaster System Example
*/

package ToasterSystem {

    // DOMAIN PILLAR - Define what exists in our system

    part def Toaster {
        doc /* A household appliance for toasting bread */

        part heatingElements : HeatingElement[2];
        part lever : Lever;
        part timer : Timer;
        part crumbTray : CrumbTray;
        part powerCord : PowerCord;

        attribute maxTemp : Real;
        attribute slots : Integer = 2;
        attribute voltage : Real = 120.0;
    }

    part def HeatingElement {
        doc /* Resistive heating coil */
        attribute power : Real;
        attribute temperature : Real;
    }

    part def Lever {
        doc /* User control for lowering bread */
        attribute position : LeverPosition;
    }

    enum def LeverPosition {
        up; down;
    }

    part def Timer {
        doc /* Controls toasting duration */
        attribute duration : Real;
        attribute setting : Integer;
    }

    part def CrumbTray {
        doc /* Removable tray for crumbs */
        attribute capacity : Real;
    }

    part def PowerCord {
        doc /* Electrical connection */
        attribute length : Real;
    }

    // BEHAVIOR PILLAR - Define what the system does

    action def ToastBread {
        doc /* Main toasting operation */
        in item bread : Bread;
        out item toast : Toast;

        action lowerBread;
        action heatElements;
        action monitorTime;
        action raiseToast;

        // Sequential flow
        first lowerBread;
        then heatElements;
        then monitorTime;
        then raiseToast;
    }

    action def CleanToaster {
        doc /* Maintenance operation */
        action removeTray;
        action emptyTray;
        action replaceTray;

        first removeTray;
        then emptyTray;
        then replaceTray;
    }

    part def Bread {
        attribute slices : Integer;
        attribute thickness : Real;
    }

    part def Toast {
        attribute brownness : BrownLevel;
    }

    enum def BrownLevel {
        light; medium; dark;
    }

    // REQUIREMENT PILLAR - Define what the system must satisfy

    requirement def SafetyRequirements {
        doc /* Safety specifications */

        requirement autoShutoff {
            doc /* Toaster must auto-shutoff */
            subject toaster : Toaster;
        require constraint { toaster::maxTemp <= 250.0 }
    }

    requirement coolTouch {
        doc /* Exterior must stay cool */
        attribute maxExteriorTemp : Real;
    }
}

requirement def PerformanceRequirements {
    doc /* Performance specifications */

    requirement toastingTime {
        doc /* Toasting duration requirement */
        attribute minDuration : Real;
        attribute maxDuration : Real;
    }

    requirement evenToasting {
        doc /* Quality requirement */
        attribute uniformity : Real;
    }
}

// ANALYSIS PILLAR - Define evaluations and calculations

calc def PowerConsumption {
    doc /* Calculate total power usage */
    in elementPower : Real[2];
    return totalPower : Real;
}

analysis def ThermalAnalysis {
    doc /* Analyze heating performance */
    subject toaster : Toaster;

    objective heatDistribution {
        doc /* Ensure even heat distribution */
    }

    calc powerCalc : PowerConsumption;
}

// INTERCONNECTION PILLAR - Define how things connect

connection def PowerConnection {
    doc /* Electrical power interface */
}

interface def ControlInterface {
    doc /* User control interface */
    attribute controlSetting : Integer;
    attribute operationState : String;
}

port def ElectricalPort {
    doc /* Power delivery port */
    attribute portVoltage : Real;
    attribute portCurrent : Real;
}

part def PowerSource;
part def ElectricalLoad;

// ALLOCATION PILLAR - Define assignments and mappings

allocation def FunctionAllocation {
    doc /* Map functions to hardware */
}

// CROSSCUTTING PILLAR - Define concerns spanning multiple aspects

metadata def Manufacturer {
    doc /* Manufacturing metadata */
    attribute company : String;
    attribute model : String;
    attribute warranty : Real;
}

comment about Toaster
/* This is a basic two-slot toaster designed for home use.
It features adjustable browning control and automatic pop-up. */

// Instantiate the system
part myToaster : Toaster {
    :>> maxTemp = 230.0;
    :>> slots = 2;
}
}
