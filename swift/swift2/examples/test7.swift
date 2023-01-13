public class GeoManager : NSObject, CLLocationManagerDelegate {


    //MARK: - Properties
    public var locationManager:CLLocationManager = CLLocationManager()
    //private(set)  var location:CLLocation?
    public var location:CLLocation?
    var locationAuthorized = false
    public class var sharedInstance: GeoManager {
    struct SharedInstance {
        static let instance = GeoManager()
        }
        return SharedInstance.instance
    }


    //MARK: - Init & deinit
    override init() {
        super.init()

        NSNotificationCenter.defaultCenter().addObserver(self, selector: "didEnterBackground:",
            name: UIApplicationDidEnterBackgroundNotification, object: nil)
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "didEnterBackground:",
            name: UIApplicationWillTerminateNotification, object: nil)
        NSNotificationCenter.defaultCenter().addObserver(self, selector: "didEnterForeground:",
            name: UIApplicationWillEnterForegroundNotification, object: nil)

    }

    deinit {
        NSNotificationCenter.defaultCenter().removeObserver(self)
    }

    //MARK: - Start & Stop
    public func start() {

        self.locationManager.requestWhenInUseAuthorization()
        if self.isLocatingAllowed() {
            self.locationManager.delegate = self
            self.locationManager.desiredAccuracy = kCLLocationAccuracyHundredMeters
            self.locationManager.startUpdatingLocation()
        }
    }

    public func stop() {
        self.locationManager.stopUpdatingLocation()
        self.locationManager.delegate = nil
    }

    func isLocatingAllowed() -> Bool {
        var allowed = true
        if CLLocationManager.locationServicesEnabled() == false {
            allowed = false
        }
        if CLLocationManager.authorizationStatus() == CLAuthorizationStatus.Denied {
            allowed = false
        }

        return allowed
    }

    //MARK: - CLLocationManagerDelegate
    public func locationManager(manager: CLLocationManager!, didUpdateLocations locations: [AnyObject]!) {

        if locations.count > 0 && self.location == nil {
            self.willChangeValueForKey("location")
            self.location = locations[0] as? CLLocation
            self.didChangeValueForKey("location")
            self.locationManager.stopUpdatingLocation()
        }
    }

    public func locationManager(manager: CLLocationManager!, didFailWithError error: NSError!) {

    }

    public func locationManager(manager: CLLocationManager!, didChangeAuthorizationStatus status: CLAuthorizationStatus) {
        if (!self.locationAuthorized && status == .AuthorizedWhenInUse) {
            // Location service was not enabled before - now start the location service again
            self.start()
        }
    }

    //MARK: - Notifications
    func didEnterBackground(notification:AnyObject) {
        self.locationManager.stopUpdatingLocation()
        // also destroy the latest location - we refresh when coming into foreground
        self.location = nil
    }

    func didEnterForeground(notification:AnyObject) {
        self.locationManager.startUpdatingLocation()
    }
}