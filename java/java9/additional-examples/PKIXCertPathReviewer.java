package org.bouncycastle.x509;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.net.HttpURLConnection;
import java.net.InetAddress;
import java.net.URL;
import java.security.GeneralSecurityException;
import java.security.PublicKey;
import java.security.cert.CertPath;
import java.security.cert.CertPathValidatorException;
import java.security.cert.CertificateExpiredException;
import java.security.cert.CertificateFactory;
import java.security.cert.CertificateNotYetValidException;
import java.security.cert.PKIXCertPathChecker;
import java.security.cert.PKIXParameters;
import java.security.cert.PolicyNode;
import java.security.cert.TrustAnchor;
import java.security.cert.X509CRL;
import java.security.cert.X509CRLEntry;
import java.security.cert.X509CRLSelector;
import java.security.cert.X509CertSelector;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javax.security.auth.x500.X500Principal;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DEREnumerated;
import org.bouncycastle.asn1.DERIA5String;
import org.bouncycastle.asn1.DERInteger;
import org.bouncycastle.asn1.DERObject;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.x509.AccessDescription;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.AuthorityInformationAccess;
import org.bouncycastle.asn1.x509.BasicConstraints;
import org.bouncycastle.asn1.x509.CRLDistPoint;
import org.bouncycastle.asn1.x509.DistributionPoint;
import org.bouncycastle.asn1.x509.DistributionPointName;
import org.bouncycastle.asn1.x509.GeneralName;
import org.bouncycastle.asn1.x509.GeneralNames;
import org.bouncycastle.asn1.x509.GeneralSubtree;
import org.bouncycastle.asn1.x509.IssuingDistributionPoint;
import org.bouncycastle.asn1.x509.NameConstraints;
import org.bouncycastle.asn1.x509.PolicyInformation;
import org.bouncycastle.asn1.x509.X509Extensions;
import org.bouncycastle.asn1.x509.X509Name;
import org.bouncycastle.asn1.x509.qualified.Iso4217CurrencyCode;
import org.bouncycastle.asn1.x509.qualified.MonetaryValue;
import org.bouncycastle.asn1.x509.qualified.QCStatement;
import org.bouncycastle.i18n.ErrorBundle;
import org.bouncycastle.i18n.filter.UntrustedInput;
import org.bouncycastle.jce.provider.CertPathValidatorUtilities;
import org.bouncycastle.jce.provider.PKIXPolicyNode;
import org.bouncycastle.jce.provider.AnnotatedException;

/**
 * PKIXCertPathReviewer<br>
 * Validation of X.509 Certificate Paths. Tries to find as much errors in the Path as possible.
 */
public class PKIXCertPathReviewer extends CertPathValidatorUtilities
{
    
    private static final String QC_STATEMENT = X509Extensions.QCStatements.getId();
    private static final String CRL_DIST_POINTS = X509Extensions.CRLDistributionPoints.getId();
    private static final String AUTH_INFO_ACCESS = X509Extensions.AuthorityInfoAccess.getId();
    
    private static final String RESOURCE_NAME = "org.bouncycastle.x509.CertPathReviewerMessages";
    
    // input parameters
    
    protected CertPath certPath;

    protected PKIXParameters pkixParams;

    protected Date validDate;

    // state variables
    
    protected List certs;

    protected int n;
    
    // output variables
    
    protected List[] notifications;
    protected List[] errors;
    protected TrustAnchor trustAnchor;
    protected PublicKey subjectPublicKey;
    protected PolicyNode policyTree;
    
    
    /**
     * Creates a PKIXCertPathReviewer for the given {@link CertPath} and {@link PKIXParameters} params
     * @param certPath the {@link CertPath} to validate
     * @param params the {@link PKIXParameters} to use
     * @throws CertPathReviewerException if the certPath is empty
     */
    public PKIXCertPathReviewer(CertPath certPath, PKIXParameters params)
            throws CertPathReviewerException
    {
        // check input parameters
        if (certPath == null)
        {
            throw new NullPointerException("certPath was null");
        }
        this.certPath = certPath;

        certs = certPath.getCertificates();
        n = certs.size();
        if (certs.isEmpty())
        {
            throw new CertPathReviewerException(
                    new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.emptyCertPath"));
        }

        pkixParams = (PKIXParameters) params.clone();

        // 6.1.1 - Inputs

        // a) done

        // b)

        validDate = getValidDate(pkixParams);

        // c) part of pkixParams

        // d) done at the beginning of checkSignatures

        // e) f) g) part of pkixParams
        
        // initialize output parameters
        
        notifications = null;
        errors = null;
        trustAnchor = null;
        subjectPublicKey = null;
        policyTree = null;

    }
    
    /**
     * 
     * @return the CertPath that was validated
     */
    public CertPath getCertPath()
    {
        return certPath;
    }
    
    /**
     * 
     * @return the size of the CertPath
     */
    public int getCertPathSize()
    {
        return n;
    }

    /**
     * Returns an Array of Lists which contains a List of global error messages 
     * and a List of error messages for each certificate in the path.
     * The global error List is at index 0. The error lists for each certificate at index 1 to n. 
     * The error messages are of type.
     * @return the Array of Lists which contain the error messages
     */
    public List[] getErrors()
    {
        doChecks();
        return errors;
    }
    
    /**
     * Returns an List of error messages for the certificate at the given index in the CertPath.
     * If index == -1 then the list of global errors is returned with errors not specific to a certificate. 
     * @param index the index of the certificate in the CertPath
     * @return List of error messages for the certificate
     */
    public List getErrors(int index)
    {
        doChecks();
        return errors[index + 1];
    }

    /**
     * Returns an Array of Lists which contains a List of global notification messages 
     * and a List of botification messages for each certificate in the path.
     * The global notificatio List is at index 0. The notification lists for each certificate at index 1 to n. 
     * The error messages are of type.
     * @return the Array of Lists which contain the notification messages
     */
    public List[] getNotifications()
    {
        doChecks();
        return notifications;
    }
    
    /**
     * Returns an List of notification messages for the certificate at the given index in the CertPath.
     * If index == -1 then the list of global notifications is returned with notifications not specific to a certificate. 
     * @param index the index of the certificate in the CertPath
     * @return List of notification messages for the certificate
     */
    public List getNotifications(int index)
    {
        doChecks();
        return notifications[index + 1];
    }

    /**
     * 
     * @return the valid policy tree, <b>null</b> if no valid policy exists.
     */
    public PolicyNode getPolicyTree()
    {
        doChecks();
        return policyTree;
    }

    /**
     * 
     * @return the PublicKey if the last certificate in the CertPath
     */
    public PublicKey getSubjectPublicKey()
    {
        doChecks();
        return subjectPublicKey;
    }

    /**
     * 
     * @return the TrustAnchor for the CertPath, <b>null</b> if no valid TrustAnchor was found.
     */
    public TrustAnchor getTrustAnchor()
    {
        doChecks();
        return trustAnchor;
    }
    
    /**
     * 
     * @return if the CertPath is valid
     */
    public boolean isValidCertPath()
    {
        doChecks();
        boolean valid = true;
        for (int i = 0; i < errors.length; i++)
        {
            if (!errors[i].isEmpty())
            {
                valid = false;
                break;
            }
        }
        return valid;
    }
    
    protected void addNotification(ErrorBundle msg)
    {
        notifications[0].add(msg);
    }
    
    protected void addNotification(ErrorBundle msg, int index)
    {
        if (index < -1 || index >= n)
        {
            throw new IndexOutOfBoundsException();
        }
        notifications[index + 1].add(msg);
    }

    protected void addError(ErrorBundle msg) 
    {
        errors[0].add(msg);
    }
    
    protected void addError(ErrorBundle msg, int index)
    {
        if (index < -1 || index >= n)
        {
            throw new IndexOutOfBoundsException();
        }
        errors[index + 1].add(msg);
    }
    
    protected void doChecks()
    {
        if (notifications == null)
        {
            // initialize lists
            notifications = new List[n+1];
            errors = new List[n+1];
            
            for (int i = 0; i < notifications.length; i++)
            {
                notifications[i] = new ArrayList();
                errors[i] = new ArrayList();
            }
            
            // check Signatures
            checkSignatures();
            
            // check Name Constraints
            checkNameConstraints();
            
            // check Path Length
            checkPathLength();
            
            // check Policy
            checkPolicy();
            
            // check other critical extensions
            checkCriticalExtensions();
            
        }
    }

    private void checkNameConstraints()
    {
        X509Certificate cert = null;
        
        //
        // Setup
        //
        
        // (b)
        Set     permittedSubtreesDN = new HashSet();
        Set     permittedSubtreesEmail = new HashSet();
        Set     permittedSubtreesIP = new HashSet();
    
        // (c)
        Set     excludedSubtreesDN = new HashSet();
        Set     excludedSubtreesEmail = new HashSet();
        Set     excludedSubtreesIP = new HashSet();

        //
        // process each certificate except the last in the path
        //
        int index;
        int i;
        
        try 
        {
            for (index = certs.size()-1; index>0; index--) 
            {
                i = n - index;
                
                //
                // certificate processing
                //    
                
                cert = (X509Certificate) certs.get(index);
                
                // b),c)
                
                if (!isSelfIssued(cert))
                {
                    X500Principal principal = getSubjectPrincipal(cert);
                    ASN1InputStream aIn = new ASN1InputStream(new ByteArrayInputStream(principal.getEncoded()));
                    ASN1Sequence    dns;
    
                    try
                    {
                        dns = (ASN1Sequence)aIn.readObject();
                    }
                    catch (IOException e)
                    {
                        ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.ncSubjectNameError", 
                                new Object[] {new UntrustedInput(principal)});
                        throw new CertPathReviewerException(msg,e,certPath,index);
                    }
    
                    try
                    {
                        checkPermittedDN(permittedSubtreesDN, dns);
                    }
                    catch (CertPathValidatorException cpve)
                    {
                        ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.notPermittedDN", 
                                new Object[] {new UntrustedInput(principal.getName())});
                        throw new CertPathReviewerException(msg,cpve,certPath,index);
                    }
                    
                    try
                    {
                        checkExcludedDN(excludedSubtreesDN, dns);
                    }
                    catch (CertPathValidatorException cpve)
                    {
                        ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.excludedDN",
                                new Object[] {new UntrustedInput(principal.getName())});
                        throw new CertPathReviewerException(msg,cpve,certPath,index);
                    }
            
                    ASN1Sequence altName;
                    try 
                    {
                        altName = (ASN1Sequence)getExtensionValue(cert, SUBJECT_ALTERNATIVE_NAME);
                    }
                    catch (AnnotatedException ae)
                    {
                        ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.subjAltNameExtError");
                        throw new CertPathReviewerException(msg,ae,certPath,index);
                    }
                    
                    if (altName != null)
                    {
                        for (int j = 0; j < altName.size(); j++)
                        {
                            ASN1TaggedObject o = (ASN1TaggedObject)altName.getObjectAt(j);
    
                            switch(o.getTagNo())
                            {
                            case 1:
                                String email = DERIA5String.getInstance(o, true).getString();
    
                                try
                                {
                                    checkPermittedEmail(permittedSubtreesEmail, email);
                                }
                                catch (CertPathValidatorException cpve)
                                {
                                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.notPermittedEmail", 
                                            new Object[] {new UntrustedInput(email)});
                                    throw new CertPathReviewerException(msg,cpve,certPath,index);
                                }
                                
                                try
                                {
                                    checkExcludedEmail(excludedSubtreesEmail, email);
                                }
                                catch (CertPathValidatorException cpve)
                                {
                                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.excludedEmail", 
                                            new Object[] {new UntrustedInput(email)});
                                    throw new CertPathReviewerException(msg,cpve,certPath,index);
                                }

                                break;
                            case 4:
                                ASN1Sequence altDN = ASN1Sequence.getInstance(o, true);
    
                                try
                                {
                                    checkPermittedDN(permittedSubtreesDN, altDN);
                                }
                                catch (CertPathValidatorException cpve)
                                {
                                    X509Name altDNName = new X509Name(altDN);
                                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.notPermittedDN", 
                                            new Object[] {new UntrustedInput(altDNName)});
                                    throw new CertPathReviewerException(msg,cpve,certPath,index);
                                }
                                
                                try
                                {
                                    checkExcludedDN(excludedSubtreesDN, altDN);
                                }
                                catch (CertPathValidatorException cpve)
                                {
                                    X509Name altDNName = new X509Name(altDN);
                                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.excludedDN", 
                                            new Object[] {new UntrustedInput(altDNName)});
                                    throw new CertPathReviewerException(msg,cpve,certPath,index);
                                }
                                
                                break;
                            case 7:
                                byte[] ip = ASN1OctetString.getInstance(o, true).getOctets();
    
                                try
                                {
                                    checkPermittedIP(permittedSubtreesIP, ip);
                                }
                                catch (CertPathValidatorException cpve)
                                {
                                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.notPermittedIP", 
                                            new Object[] {IPtoString(ip)});
                                    throw new CertPathReviewerException(msg,cpve,certPath,index);
                                }
                                
                                try
                                {
                                    checkExcludedIP(excludedSubtreesIP, ip);
                                }
                                catch (CertPathValidatorException cpve)
                                {
                                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.excludedIP", 
                                            new Object[] {IPtoString(ip)});
                                    throw new CertPathReviewerException(msg,cpve,certPath,index);
                                }
                            }
                        }
                    }
                }
                
                //
                // prepare for next certificate
                //
                
                //
                // (g) handle the name constraints extension
                //
                ASN1Sequence ncSeq;
                try 
                {
                    ncSeq = (ASN1Sequence)getExtensionValue(cert, NAME_CONSTRAINTS);
                }
                catch (AnnotatedException ae)
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.ncExtError");
                    throw new CertPathReviewerException(msg,ae,certPath,index);
                }
                
                if (ncSeq != null)
                {
                    NameConstraints nc = new NameConstraints(ncSeq);

                    //
                    // (g) (1) permitted subtrees
                    //
                    ASN1Sequence permitted = nc.getPermittedSubtrees();
                    if (permitted != null)
                    {
                        Enumeration e = permitted.getObjects();
                        while (e.hasMoreElements())
                        {
                            GeneralSubtree  subtree = GeneralSubtree.getInstance(e.nextElement());
                            GeneralName     base = subtree.getBase();

                            switch(base.getTagNo())
                            {
                                case 1:
                                    permittedSubtreesEmail = intersectEmail(permittedSubtreesEmail, DERIA5String.getInstance(base.getName()).getString());
                                    break;
                                case 4:
                                    permittedSubtreesDN = intersectDN(permittedSubtreesDN, (ASN1Sequence)base.getName());
                                    break;
                                case 7:
                                    permittedSubtreesIP = intersectIP(permittedSubtreesIP, ASN1OctetString.getInstance(base.getName()).getOctets());
                                    break;
                            }
                        }
                    }
                
                    //
                    // (g) (2) excluded subtrees
                    //
                    ASN1Sequence excluded = nc.getExcludedSubtrees();
                    if (excluded != null)
                    {
                        Enumeration e = excluded.getObjects();
                        while (e.hasMoreElements())
                        {
                            GeneralSubtree  subtree = GeneralSubtree.getInstance(e.nextElement());
                            GeneralName     base = subtree.getBase();

                            switch(base.getTagNo())
                            {
                            case 1:
                                excludedSubtreesEmail = unionEmail(excludedSubtreesEmail, DERIA5String.getInstance(base.getName()).getString());
                                break;
                            case 4:
                                excludedSubtreesDN = unionDN(excludedSubtreesDN, (ASN1Sequence)base.getName());
                                break;
                            case 7:
                                excludedSubtreesIP = unionIP(excludedSubtreesIP, ASN1OctetString.getInstance(base.getName()).getOctets());
                                break;
                            }
                        }
                    }
                }
                
            } // for
        }
        catch (CertPathReviewerException cpre)
        {
            addError(cpre.getErrorMessage(),cpre.getIndex());
        }
        
    }

    /*
     * checks: - path length constraints and reports - total path length
     */
    private void checkPathLength()
    {
        // init
        int maxPathLength = n;
        int totalPathLength = 0;

        X509Certificate cert = null;

        int i;
        for (int index = certs.size() - 1; index > 0; index--)
        {
            i = n - index;

            cert = (X509Certificate) certs.get(index);

            // l)

            if (!isSelfIssued(cert))
            {
                if (maxPathLength <= 0)
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.pathLenghtExtended");
                    addError(msg);
                }
                maxPathLength--;
                totalPathLength++;
            }

            // m)

            BasicConstraints bc;
            try
            {
                bc = BasicConstraints.getInstance(getExtensionValue(cert,
                        BASIC_CONSTRAINTS));
            }
            catch (AnnotatedException ae)
            {
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.processLengthConstError");
                addError(msg,index);
                bc = null;
            }

            if (bc != null)
            {
                BigInteger _pathLengthConstraint = bc.getPathLenConstraint();

                if (_pathLengthConstraint != null)
                {
                    int _plc = _pathLengthConstraint.intValue();

                    if (_plc < maxPathLength)
                    {
                        maxPathLength = _plc;
                    }
                }
            }

        }

        ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.totalPathLength",
                new Object[] {new Integer(totalPathLength)});
        
        addNotification(msg);
    }

    /*
     * checks: - signatures - name chaining - validity of certificates - todo:
     * if certificate revoked (if specified in the parameters)
     */
    private void checkSignatures()
    {
        // 1.6.1 - Inputs
        
        // d)
        
        TrustAnchor trust = null;
        X500Principal trustPrincipal = null;
        
        // validation date
        {
            ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.certPathValidDate",
                    new Object[] {validDate, new Date()});
            addNotification(msg);
        }
        
        // find trust anchors
        try
        {
            X509Certificate cert = (X509Certificate) certs.get(certs.size() - 1);
            Collection trustColl = getTrustAnchors(cert,pkixParams.getTrustAnchors());
            if (trustColl.size() > 1)
            {
                // conflicting trust anchors                
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,
                        "CertPathReviewer.conflictingTrustAnchors",
                        new Object[] {new Integer(trustColl.size()),
                                      new UntrustedInput(cert.getIssuerX500Principal())});
                addError(msg);
            }
            else if (trustColl.isEmpty())
            {
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,
                        "CertPathReviewer.noTrustAnchorFound",
                        new Object[] {new UntrustedInput(cert.getIssuerX500Principal()),
                                      new Integer(pkixParams.getTrustAnchors().size())});
                addError(msg);
            }
            else
            {
                PublicKey trustPublicKey;
                trust = (TrustAnchor) trustColl.iterator().next();
                if (trust.getTrustedCert() != null)
                {
                    trustPublicKey = trust.getTrustedCert().getPublicKey();
                }
                else
                {
                    trustPublicKey = trust.getCAPublicKey();
                }
                try
                {
                    cert.verify(trustPublicKey);
                }
                catch (Exception e)
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.trustButInvalidCert");
                    addError(msg);
                    trust = null;
                }
            }
        }
        catch (CertPathReviewerException cpre)
        {
            addError(cpre.getErrorMessage());
        }
        
        if (trust != null)
        {
            // get the name of the trustAnchor
            X509Certificate sign = trust.getTrustedCert();
            try
            {
                if (sign != null)
                {
                    trustPrincipal = getSubjectPrincipal(sign);
                }
                else
                {
                    trustPrincipal = new X500Principal(trust.getCAName());
                }
            }
            catch (IllegalArgumentException ex)
            {
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.trustDNInvalid",
                        new Object[] {new UntrustedInput(trust.getCAName())});
                addError(msg);
            }
        }
        
        // 1.6.2 - Initialization
        
        PublicKey workingPublicKey = null;
        X500Principal workingIssuerName = trustPrincipal;
        
        X509Certificate sign = null;

        AlgorithmIdentifier workingAlgId = null;
        DERObjectIdentifier workingPublicKeyAlgorithm = null;
        DEREncodable workingPublicKeyParameters = null;
        
        if (trust != null)
        {
            sign = trust.getTrustedCert();
            
            if (sign != null)
            {
                workingPublicKey = sign.getPublicKey();
            }
            else
            {
                workingPublicKey = trust.getCAPublicKey();
            }
        
            try
            {
                workingAlgId = getAlgorithmIdentifier(workingPublicKey);
                workingPublicKeyAlgorithm = workingAlgId.getObjectId();
                workingPublicKeyParameters = workingAlgId.getParameters();
            }
            catch (CertPathValidatorException ex)
            {
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.trustPubKeyError");
                addError(msg);
                workingAlgId = null;
            }
            
        }

        // Basic cert checks

        X509Certificate cert = null;
        int i;

        for (int index = certs.size() - 1; index >= 0; index--)
        {
            //
            // i as defined in the algorithm description
            //
            i = n - index;

            //
            // set certificate to be checked in this round
            // sign and workingPublicKey and workingIssuerName are set
            // at the end of the for loop and initialied the
            // first time from the TrustAnchor
            //
            cert = (X509Certificate) certs.get(index);

            // verify signature
            if (workingPublicKey != null)
            {
                try
                {
                    cert.verify(workingPublicKey, "BC");
                }
                catch (GeneralSecurityException ex)
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.signatureNotVerified",
                            new Object[] {ex.getMessage(),ex}); 
                    addError(msg,index);
                }
            }
            else
            {
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.NoIssuerPublicKey");
                addError(msg,index);
            }

            // certificate valid?
            try
            {
                cert.checkValidity(validDate);
            }
            catch (CertificateNotYetValidException cnve)
            {
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.certificateNotYetValid",
                        new Object[] {cert.getNotBefore()});
                addError(msg,index);
            }
            catch (CertificateExpiredException cee)
            {
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.certificateExpired",
                        new Object[] {cert.getNotAfter()});
                addError(msg,index);
            }

            // certificate revoked?
            if (pkixParams.isRevocationEnabled())
            {
                // read crl distribution points extension
                CRLDistPoint crlDistPoints = null;
                try
                {
                    DERObject crl_dp = getExtensionValue(cert,CRL_DIST_POINTS);
                    if (crl_dp != null)
                    {
                        crlDistPoints = CRLDistPoint.getInstance(crl_dp);
                    }
                }
                catch (AnnotatedException ae)
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.crlDistPtExtError");
                    addError(msg,index);
                }

                // read authority information access extension
                AuthorityInformationAccess authInfoAcc = null;
                try
                {
                    DERObject auth_info_acc = getExtensionValue(cert,AUTH_INFO_ACCESS);
                    if (auth_info_acc != null)
                    {
                        authInfoAcc = AuthorityInformationAccess.getInstance(auth_info_acc);
                    }
                }
                catch (AnnotatedException ae)
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.crlAuthInfoAccError");
                    addError(msg,index);
                }
                
                Vector crlDistPointUrls = getCRLDistUrls(crlDistPoints,authInfoAcc);
                Vector ocspUrls = getOCSPUrls(authInfoAcc);
                
                // add notifications with the crl distribution points
                
                // output crl distribution points
                Iterator urlIt = crlDistPointUrls.iterator();
                while (urlIt.hasNext())
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.crlDistPoint",
                                new Object[] {new UntrustedInput(urlIt.next())});
                    addNotification(msg,index);
                }
                
                // output ocsp urls
                urlIt = ocspUrls.iterator();
                while (urlIt.hasNext())
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.ocspLocation",
                            new Object[] {new UntrustedInput(urlIt.next())});
                    addNotification(msg,index);
                }
                
                // TODO also support Netscapes revocation-url and/or OCSP instead of CRLs for revocation checking
                // check CRLs
                try 
                {
                    checkCRLs(pkixParams, cert, validDate, sign, workingPublicKey, crlDistPointUrls, index);
                }
                catch (CertPathReviewerException cpre)
                {
                    addError(cpre.getErrorMessage(),index);
                }
            }

            // certificate issuer correct
            if (workingIssuerName != null && !cert.getIssuerX500Principal().equals(workingIssuerName))
            {
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.certWrongIssuer",
                            new Object[] {workingIssuerName.getName(),
                            cert.getIssuerX500Principal().getName()});
                addError(msg,index);
            }

            //
            // prepare for next certificate
            //
            if (i != n)
            {

                if (cert != null && cert.getVersion() == 1)
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.noCACert");
                    addError(msg,index);
                }

                // k)

                BasicConstraints bc;
                try
                {
                    bc = BasicConstraints.getInstance(getExtensionValue(cert,
                            BASIC_CONSTRAINTS));
                    if (bc != null)
                    {
                        if (!bc.isCA())
                        {
                            ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.noCACert");
                            addError(msg,index);
                        }
                    }
                    else
                    {
                        ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.noBasicConstraints");
                        addError(msg,index);
                    }
                }
                catch (AnnotatedException ae)
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.errorProcesingBC");
                    addError(msg,index);
                }

                // n)

                boolean[] _usage = cert.getKeyUsage();

                if ((_usage != null) && !_usage[KEY_CERT_SIGN])
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.noCertSign");
                    addError(msg,index);
                }

            } // if

            // set signing certificate for next round
            sign = cert;
            
            // c)

            workingIssuerName = cert.getSubjectX500Principal();

            // d)

            workingPublicKey = cert.getPublicKey();

            // e) f)

            try
            {
                workingAlgId = getAlgorithmIdentifier(workingPublicKey);
                workingPublicKeyAlgorithm = workingAlgId.getObjectId();
                workingPublicKeyParameters = workingAlgId.getParameters();
            }
            catch (CertPathValidatorException ex)
            {
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.pubKeyError");
                addError(msg,index);
                workingAlgId = null;
                workingPublicKeyAlgorithm = null;
                workingPublicKeyParameters = null;
            }

        } // for

        trustAnchor = trust;
        subjectPublicKey = workingPublicKey;
    }

    private void checkPolicy()
    {
        //
        // 6.1.1 Inputs
        //

        // c) Initial Policy Set

        Set userInitialPolicySet = pkixParams.getInitialPolicies();

        // e) f) g) are part of pkixParams

        //
        // 6.1.2 Initialization
        //

        // a) valid policy tree

        List[] policyNodes = new ArrayList[n + 1];
        for (int j = 0; j < policyNodes.length; j++)
        {
            policyNodes[j] = new ArrayList();
        }

        Set policySet = new HashSet();

        policySet.add(ANY_POLICY);

        PKIXPolicyNode validPolicyTree = new PKIXPolicyNode(new ArrayList(), 0,
                policySet, null, new HashSet(), ANY_POLICY, false);

        policyNodes[0].add(validPolicyTree);

        // d) explicit policy

        int explicitPolicy;
        if (pkixParams.isExplicitPolicyRequired())
        {
            explicitPolicy = 0;
        }
        else
        {
            explicitPolicy = n + 1;
        }

        // e) inhibit any policy

        int inhibitAnyPolicy;
        if (pkixParams.isAnyPolicyInhibited())
        {
            inhibitAnyPolicy = 0;
        }
        else
        {
            inhibitAnyPolicy = n + 1;
        }

        // f) policy mapping

        int policyMapping;
        if (pkixParams.isPolicyMappingInhibited())
        {
            policyMapping = 0;
        }
        else
        {
            policyMapping = n + 1;
        }

        Set acceptablePolicies = null;

        //
        // 6.1.3 Basic Certificate processing
        //

        X509Certificate cert = null;
        int index;
        int i;

        try 
        {
            for (index = certs.size() - 1; index >= 0; index--)
            {
                // i as defined in the algorithm description
                i = n - index;
    
                // set certificate to be checked in this round
                cert = (X509Certificate) certs.get(index);
    
                // d) process policy information
    
                ASN1Sequence certPolicies;
                try 
                {
                    certPolicies = (ASN1Sequence) getExtensionValue(
                        cert, CERTIFICATE_POLICIES);
                }
                catch (AnnotatedException ae)
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.policyExtError");
                    throw new CertPathReviewerException(msg,ae,certPath,index);
                }
                if (certPolicies != null && validPolicyTree != null)
                {

                    // d) 1)

                    Enumeration e = certPolicies.getObjects();
                    Set pols = new HashSet();

                    while (e.hasMoreElements())
                    {
                        PolicyInformation pInfo = PolicyInformation.getInstance(e.nextElement());
                        DERObjectIdentifier pOid = pInfo.getPolicyIdentifier();

                        pols.add(pOid.getId());

                        if (!ANY_POLICY.equals(pOid.getId()))
                        {
                            Set pq;
                            try
                            {
                                pq = getQualifierSet(pInfo.getPolicyQualifiers());
                            }
                            catch (CertPathValidatorException cpve)
                            {
                                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.policyQualifierError");
                                throw new CertPathReviewerException(msg,cpve,certPath,index);
                            }

                            boolean match = processCertD1i(i, policyNodes, pOid, pq);

                            if (!match)
                            {
                                processCertD1ii(i, policyNodes, pOid, pq);
                            }
                        }
                    }

                    if (acceptablePolicies == null || acceptablePolicies.contains(ANY_POLICY))
                    {
                        acceptablePolicies = pols;
                    }
                    else
                    {
                        Iterator it = acceptablePolicies.iterator();
                        Set t1 = new HashSet();

                        while (it.hasNext())
                        {
                            Object o = it.next();

                            if (pols.contains(o))
                            {
                                t1.add(o);
                            }
                        }

                        acceptablePolicies = t1;
                    }

                    // d) 2)

                    if ((inhibitAnyPolicy > 0) || ((i < n) && isSelfIssued(cert)))
                    {
                        e = certPolicies.getObjects();

                        while (e.hasMoreElements())
                        {
                            PolicyInformation pInfo = PolicyInformation.getInstance(e.nextElement());

                            if (ANY_POLICY.equals(pInfo.getPolicyIdentifier().getId()))
                            {
                                Set _apq;
                                try
                                {
                                    _apq = getQualifierSet(pInfo.getPolicyQualifiers());
                                }
                                catch (CertPathValidatorException cpve)
                                {
                                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.policyQualifierError");
                                    throw new CertPathReviewerException(msg,cpve,certPath,index);
                                }
                                List _nodes = policyNodes[i - 1];

                                for (int k = 0; k < _nodes.size(); k++)
                                {
                                    PKIXPolicyNode _node = (PKIXPolicyNode) _nodes.get(k);

                                    Iterator _policySetIter = _node.getExpectedPolicies().iterator();
                                    while (_policySetIter.hasNext())
                                    {
                                        Object _tmp = _policySetIter.next();

                                        String _policy;
                                        if (_tmp instanceof String)
                                        {
                                            _policy = (String) _tmp;
                                        }
                                        else if (_tmp instanceof DERObjectIdentifier)
                                        {
                                            _policy = ((DERObjectIdentifier) _tmp).getId();
                                        }
                                        else
                                        {
                                            continue;
                                        }

                                        boolean _found = false;
                                        Iterator _childrenIter = _node
                                                .getChildren();

                                        while (_childrenIter.hasNext())
                                        {
                                            PKIXPolicyNode _child = (PKIXPolicyNode) _childrenIter.next();

                                            if (_policy.equals(_child.getValidPolicy()))
                                            {
                                                _found = true;
                                            }
                                        }

                                        if (!_found)
                                        {
                                            Set _newChildExpectedPolicies = new HashSet();
                                            _newChildExpectedPolicies.add(_policy);

                                            PKIXPolicyNode _newChild = new PKIXPolicyNode(
                                                    new ArrayList(), i,
                                                    _newChildExpectedPolicies,
                                                    _node, _apq, _policy, false);
                                            _node.addChild(_newChild);
                                            policyNodes[i].add(_newChild);
                                        }
                                    }
                                }
                                break;
                            }
                        }
                    }

                    //
                    // (d) (3)
                    //
                    for (int j = (i - 1); j >= 0; j--)
                    {
                        List nodes = policyNodes[j];

                        for (int k = 0; k < nodes.size(); k++)
                        {
                            PKIXPolicyNode node = (PKIXPolicyNode) nodes.get(k);
                            if (!node.hasChildren())
                            {
                                validPolicyTree = removePolicyNode(
                                        validPolicyTree, policyNodes, node);
                                if (validPolicyTree == null)
                                {
                                    break;
                                }
                            }
                        }
                    }

                    //
                    // d (4)
                    //
                    Set criticalExtensionOids = cert.getCriticalExtensionOIDs();

                    if (criticalExtensionOids != null)
                    {
                        boolean critical = criticalExtensionOids.contains(CERTIFICATE_POLICIES);

                        List nodes = policyNodes[i];
                        for (int j = 0; j < nodes.size(); j++)
                        {
                            PKIXPolicyNode node = (PKIXPolicyNode) nodes.get(j);
                            node.setCritical(critical);
                        }
                    }

                }
                
                // e)
                
                if (certPolicies == null) 
                {
                    validPolicyTree = null;
                }
                
                // f)
                
                if (explicitPolicy <= 0 && validPolicyTree == null)
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.noValidPolicyTree");
                    throw new CertPathReviewerException(msg);
                }
    
                //
                // 6.1.4 preparation for next Certificate
                //
    
                if (i != n)
                {
                    
                    // a)
                    
                    DERObject pm;
                    try
                    {
                        pm = getExtensionValue(cert, POLICY_MAPPINGS);
                    }
                    catch (AnnotatedException ae)
                    {
                        ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.policyMapExtError");
                        throw new CertPathReviewerException(msg,ae,certPath,index);
                    }
                    
                    if (pm != null) 
                    {
                        ASN1Sequence mappings = (ASN1Sequence) pm;
                        for (int j = 0; j < mappings.size(); j++) 
                        {
                            ASN1Sequence mapping = (ASN1Sequence) mappings.getObjectAt(j);
                            DERObjectIdentifier ip_id = (DERObjectIdentifier) mapping.getObjectAt(0);
                            DERObjectIdentifier sp_id = (DERObjectIdentifier) mapping.getObjectAt(1);
                            if (ANY_POLICY.equals(ip_id.getId())) 
                            {
                                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.invalidPolicyMapping");
                                throw new CertPathReviewerException(msg,certPath,index);
                            }
                            if (ANY_POLICY.equals(sp_id.getId()))
                            {
                                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.invalidPolicyMapping");
                                throw new CertPathReviewerException(msg,certPath,index);
                            }
                        }
                    }
                    
                    // b)
                    
                    if (pm != null)
                    {
                        ASN1Sequence mappings = (ASN1Sequence)pm;
                        Map m_idp = new HashMap();
                        Set s_idp = new HashSet();
                        
                        for (int j = 0; j < mappings.size(); j++)
                        {
                            ASN1Sequence mapping = (ASN1Sequence)mappings.getObjectAt(j);
                            String id_p = ((DERObjectIdentifier)mapping.getObjectAt(0)).getId();
                            String sd_p = ((DERObjectIdentifier)mapping.getObjectAt(1)).getId();
                            Set tmp;
                            
                            if (!m_idp.containsKey(id_p))
                            {
                                tmp = new HashSet();
                                tmp.add(sd_p);
                                m_idp.put(id_p, tmp);
                                s_idp.add(id_p);
                            }
                            else
                            {
                                tmp = (Set)m_idp.get(id_p);
                                tmp.add(sd_p);
                            }
                        }
    
                        Iterator it_idp = s_idp.iterator();
                        while (it_idp.hasNext())
                        {
                            String id_p = (String)it_idp.next();
                            
                            //
                            // (1)
                            //
                            if (policyMapping > 0)
                            {
                                try
                                {
                                    prepareNextCertB1(i,policyNodes,id_p,m_idp,cert);
                                }
                                catch (AnnotatedException ae)
                                {
                                    // error processing certificate policies extension
                                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.policyExtError");
                                    throw new CertPathReviewerException(msg,ae,certPath,index);
                                }
                                catch (CertPathValidatorException cpve)
                                {
                                    // error building qualifier set
                                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.policyQualifierError");
                                    throw new CertPathReviewerException(msg,cpve,certPath,index);
                                }
                                
                                //
                                // (2)
                                // 
                            }
                            else if (policyMapping <= 0)
                            {
                                validPolicyTree = prepareNextCertB2(i,policyNodes,id_p,validPolicyTree);
                            }
                            
                        }
                    }
                    
                    //
                    // h)
                    //
                    
                    if (!isSelfIssued(cert)) 
                    {
                        
                        // (1)
                        if (explicitPolicy != 0)
                        {
                            explicitPolicy--;
                        }
                        
                        // (2)
                        if (policyMapping != 0)
                        {
                            policyMapping--;
                        }
                        
                        // (3)
                        if (inhibitAnyPolicy != 0)
                        {
                            inhibitAnyPolicy--;
                        }
                        
                    }
    
                    //
                    // i)
                    //
                    
                    try
                    {
                        ASN1Sequence pc = (ASN1Sequence) getExtensionValue(cert,POLICY_CONSTRAINTS);
                        if (pc != null)
                        {
                            Enumeration policyConstraints = pc.getObjects();
                            
                            while (policyConstraints.hasMoreElements())
                            {
                                ASN1TaggedObject constraint = (ASN1TaggedObject) policyConstraints.nextElement();
                                int tmpInt; 
                                
                                switch (constraint.getTagNo())
                                {
                                case 0:
                                    tmpInt = DERInteger.getInstance(constraint).getValue().intValue();
                                    if (tmpInt < explicitPolicy)
                                    {
                                        explicitPolicy = tmpInt;
                                    }
                                    break;
                                case 1:
                                    tmpInt = DERInteger.getInstance(constraint).getValue().intValue();
                                    if (tmpInt < policyMapping)
                                    {
                                        policyMapping = tmpInt;
                                    }
                                break;
                                }
                            }
                        }
                    }
                    catch (AnnotatedException ae)
                    {
                        ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.policyConstExtError");
                        throw new CertPathReviewerException(msg,certPath,index);
                    }
    
                    //
                    // j)
                    //
                    
                    try 
                    {
                        DERInteger iap = (DERInteger)getExtensionValue(cert, INHIBIT_ANY_POLICY);
                        
                        if (iap != null)
                        {
                            int _inhibitAnyPolicy = iap.getValue().intValue();
                        
                            if (_inhibitAnyPolicy < inhibitAnyPolicy)
                            {
                                inhibitAnyPolicy = _inhibitAnyPolicy;
                            }
                        }
                    }
                    catch (AnnotatedException ae)
                    {
                        ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.policyInhibitExtError");
                        throw new CertPathReviewerException(msg,certPath,index);
                    }
                }
    
            }
    
            //
            // 6.1.5 Wrap up
            //
    
            //
            // a)
            //
            
            if (!isSelfIssued(cert) && explicitPolicy > 0) 
            {
                explicitPolicy--;
            }
    
            //
            // b)
            //
            
            try
            {
                ASN1Sequence pc = (ASN1Sequence) getExtensionValue(cert, POLICY_CONSTRAINTS);
                if (pc != null)
                {
                    Enumeration policyConstraints = pc.getObjects();
        
                    while (policyConstraints.hasMoreElements())
                    {
                        ASN1TaggedObject    constraint = (ASN1TaggedObject)policyConstraints.nextElement();
                        switch (constraint.getTagNo())
                        {
                        case 0:
                            int tmpInt = DERInteger.getInstance(constraint).getValue().intValue();
                            if (tmpInt == 0)
                            {
                                explicitPolicy = 0;
                            }
                            break;
                        }
                    }
                }
            }
            catch (AnnotatedException e)
            {
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.policyConstExtError");
                throw new CertPathReviewerException(msg,certPath,index);
            }
            
            
            //
            // (g)
            //
            PKIXPolicyNode intersection;
            
    
            //
            // (g) (i)
            //
            if (validPolicyTree == null)
            { 
                if (pkixParams.isExplicitPolicyRequired())
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.explicitPolicy");
                    throw new CertPathReviewerException(msg,certPath,index);
                }
                intersection = null;
            }
            else if (isAnyPolicy(userInitialPolicySet)) // (g) (ii)
            {
                if (pkixParams.isExplicitPolicyRequired())
                {
                    if (acceptablePolicies.isEmpty())
                    {
                        ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.explicitPolicy");
                        throw new CertPathReviewerException(msg,certPath,index);
                    }
                    else
                    {
                        Set _validPolicyNodeSet = new HashSet();
                        
                        for (int j = 0; j < policyNodes.length; j++)
                        {
                            List      _nodeDepth = policyNodes[j];
                            
                            for (int k = 0; k < _nodeDepth.size(); k++)
                            {
                                PKIXPolicyNode _node = (PKIXPolicyNode)_nodeDepth.get(k);
                                
                                if (ANY_POLICY.equals(_node.getValidPolicy()))
                                {
                                    Iterator _iter = _node.getChildren();
                                    while (_iter.hasNext())
                                    {
                                        _validPolicyNodeSet.add(_iter.next());
                                    }
                                }
                            }
                        }
                        
                        Iterator _vpnsIter = _validPolicyNodeSet.iterator();
                        while (_vpnsIter.hasNext())
                        {
                            PKIXPolicyNode _node = (PKIXPolicyNode)_vpnsIter.next();
                            String _validPolicy = _node.getValidPolicy();
                            
                            if (!acceptablePolicies.contains(_validPolicy))
                            {
                                //validPolicyTree = removePolicyNode(validPolicyTree, policyNodes, _node);
                            }
                        }
                        if (validPolicyTree != null)
                        {
                            for (int j = (n - 1); j >= 0; j--)
                            {
                                List      nodes = policyNodes[j];
                                
                                for (int k = 0; k < nodes.size(); k++)
                                {
                                    PKIXPolicyNode node = (PKIXPolicyNode)nodes.get(k);
                                    if (!node.hasChildren())
                                    {
                                        validPolicyTree = removePolicyNode(validPolicyTree, policyNodes, node);
                                    }
                                }
                            }
                        }
                    }
                }
    
                intersection = validPolicyTree;
            }
            else
            {
                //
                // (g) (iii)
                //
                // This implementation is not exactly same as the one described in RFC3280.
                // However, as far as the validation result is concerned, both produce 
                // adequate result. The only difference is whether AnyPolicy is remain 
                // in the policy tree or not. 
                //
                // (g) (iii) 1
                //
                Set _validPolicyNodeSet = new HashSet();
                
                for (int j = 0; j < policyNodes.length; j++)
                {
                    List      _nodeDepth = policyNodes[j];
                    
                    for (int k = 0; k < _nodeDepth.size(); k++)
                    {
                        PKIXPolicyNode _node = (PKIXPolicyNode)_nodeDepth.get(k);
                        
                        if (ANY_POLICY.equals(_node.getValidPolicy()))
                        {
                            Iterator _iter = _node.getChildren();
                            while (_iter.hasNext())
                            {
                                PKIXPolicyNode _c_node = (PKIXPolicyNode)_iter.next();
                                if (!ANY_POLICY.equals(_c_node.getValidPolicy()))
                                {
                                    _validPolicyNodeSet.add(_c_node);
                                }
                            }
                        }
                    }
                }
                
                //
                // (g) (iii) 2
                //
                Iterator _vpnsIter = _validPolicyNodeSet.iterator();
                while (_vpnsIter.hasNext())
                {
                    PKIXPolicyNode _node = (PKIXPolicyNode)_vpnsIter.next();
                    String _validPolicy = _node.getValidPolicy();
    
                    if (!userInitialPolicySet.contains(_validPolicy))
                    {
                        validPolicyTree = removePolicyNode(validPolicyTree, policyNodes, _node);
                    }
                }
                
                //
                // (g) (iii) 4
                //
                if (validPolicyTree != null)
                {
                    for (int j = (n - 1); j >= 0; j--)
                    {
                        List      nodes = policyNodes[j];
                        
                        for (int k = 0; k < nodes.size(); k++)
                        {
                            PKIXPolicyNode node = (PKIXPolicyNode)nodes.get(k);
                            if (!node.hasChildren())
                            {
                                validPolicyTree = removePolicyNode(validPolicyTree, policyNodes, node);
                            }
                        }
                    }
                }
                
                intersection = validPolicyTree;
            }
     
            if ((explicitPolicy <= 0) && (intersection == null))
            {
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.invalidPolicy");
                throw new CertPathReviewerException(msg);
            }
            
            validPolicyTree = intersection;
        }
        catch (CertPathReviewerException cpre)
        {
            addError(cpre.getErrorMessage(),cpre.getIndex());
            validPolicyTree = null;
        }
    }

    private void checkCriticalExtensions()
    {
        //      
        // initialise CertPathChecker's
        //
        List  pathCheckers = pkixParams.getCertPathCheckers();
        Iterator certIter = pathCheckers.iterator();
        
        try
        {
            try
            {
                while (certIter.hasNext())
                {
                    ((PKIXCertPathChecker)certIter.next()).init(false);
                }
            }
            catch (CertPathValidatorException cpve)
            {
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.certPathCheckerError",
                        new Object[] {cpve.getMessage(),cpve});
                throw new CertPathReviewerException(msg,cpve);
            }
            
            //
            // process critical extesions for each certificate
            //
            
            X509Certificate cert = null;
            
            int index;
            
            for (index = certs.size()-1; index >= 0; index--)
            {
                cert = (X509Certificate) certs.get(index);
                
                Set criticalExtensions = new HashSet(cert.getCriticalExtensionOIDs());
                // remove already processed extensions
                criticalExtensions.remove(KEY_USAGE);
                criticalExtensions.remove(CERTIFICATE_POLICIES);
                criticalExtensions.remove(POLICY_MAPPINGS);
                criticalExtensions.remove(INHIBIT_ANY_POLICY);
                criticalExtensions.remove(ISSUING_DISTRIBUTION_POINT);
                criticalExtensions.remove(DELTA_CRL_INDICATOR);
                criticalExtensions.remove(POLICY_CONSTRAINTS);
                criticalExtensions.remove(BASIC_CONSTRAINTS);
                criticalExtensions.remove(SUBJECT_ALTERNATIVE_NAME);
                criticalExtensions.remove(NAME_CONSTRAINTS);
                
                // process qcStatements extension
                if (criticalExtensions.contains(QC_STATEMENT))
                {
                    if (processQcStatements(cert,index)) 
                    {
                        criticalExtensions.remove(QC_STATEMENT);
                    }
                }
                
                Iterator tmpIter = pathCheckers.iterator();
                while (tmpIter.hasNext())
                {
                    try
                    {
                        ((PKIXCertPathChecker)tmpIter.next()).check(cert, criticalExtensions);
                    }
                    catch (CertPathValidatorException e)
                    {
                        ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.criticalExtensionError",
                                new Object[] {e.getMessage(),e});
                        throw new CertPathReviewerException(msg,e.getCause(),certPath,index);
                    }
                }
                if (!criticalExtensions.isEmpty())
                {
                    ErrorBundle msg;
                    if (criticalExtensions.size() == 1)
                    {
                        msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.unknownCriticalExt",
                                new Object[] {criticalExtensions.iterator().next()});
                    }
                    else
                    {
                        msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.unknownCriticalExts",
                                new Object[] {new UntrustedInput(criticalExtensions)});
                    }
                    throw new CertPathReviewerException(msg,certPath,index);
                }
            }
        }
        catch (CertPathReviewerException cpre)
        {
            addError(cpre.getErrorMessage(),cpre.getIndex());
        }
    }
    
    private boolean processQcStatements(
            X509Certificate cert,
            int index)
    {   
        try
        {
            boolean unknownStatement = false;
            
            ASN1Sequence qcSt = (ASN1Sequence) getExtensionValue(cert,QC_STATEMENT);
            for (int j = 0; j < qcSt.size(); j++)
            {
                QCStatement stmt = QCStatement.getInstance(qcSt.getObjectAt(j));
                if (QCStatement.id_etsi_qcs_QcCompliance.equals(stmt.getStatementId()))
                {
                    // process statement - just write a notification that the certificate contains this statement
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.QcEuCompliance");
                    addNotification(msg,index);
                }
                else if (QCStatement.id_qcs_pkixQCSyntax_v1.equals(stmt.getStatementId()))
                {
                    // process statement - just recognize the statement
                }
                else if (QCStatement.id_etsi_qcs_QcSSCD.equals(stmt.getStatementId()))
                {
                    // process statement - just write a notification that the certificate contains this statement
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.QcSSCD");
                    addNotification(msg,index);
                }
                else if (QCStatement.id_etsi_qcs_LimiteValue.equals(stmt.getStatementId()))
                {
                    // process statement - write a notification containing the limit value
                    MonetaryValue limit = MonetaryValue.getInstance(stmt.getStatementInfo());
                    Iso4217CurrencyCode currency = limit.getCurrency();
                    double value = limit.getAmount().doubleValue() * Math.pow(10,limit.getExponent().doubleValue());
                    ErrorBundle msg;
                    if (limit.getCurrency().isAlphabetic())
                    {
                        msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.QcLimitValueAlpha",
                                new Object[] {limit.getCurrency().getAlphabetic(),
                                              new Double(value),
                                              limit});
                    }
                    else
                    {
                        msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.QcLimitValueNum",
                                new Object[] {new Integer(limit.getCurrency().getNumeric()),
                                              new Double(value),
                                              limit});
                    }
                    addNotification(msg,index);
                }
                else
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.QcUnknownStatement",
                            new Object[] {stmt.getStatementId(),new UntrustedInput(stmt)});
                    addNotification(msg,index);
                    unknownStatement = true;
                }
            }
            
            return !unknownStatement;
        }
        catch (AnnotatedException ae)
        {
            ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.QcStatementExtError");
            addError(msg,index);
        }
        
        return false;
    }
    
    private String IPtoString(byte[] ip)
    {
        String result;
        try
        {
            result = InetAddress.getByAddress(ip).getHostAddress();
        }
        catch (Exception e)
        {
            StringBuffer b = new StringBuffer();
            
            for (int i = 0; i != ip.length; i++)
            {
                b.append(Integer.toHexString(ip[i] & 0xff));
                b.append(' ');
            }
            
            result = b.toString();
        }
        
        return result;
    }
    
    private void checkCRLs(
            PKIXParameters paramsPKIX,
            X509Certificate cert,
            Date validDate,
            X509Certificate sign,
            PublicKey workingPublicKey,
            Vector crlDistPointUrls,
            int index) 
        throws CertPathReviewerException
    {
        X509CRLSelector crlselect;
        crlselect = new X509CRLSelector();
        
        try
        {
            crlselect.addIssuerName(getEncodedIssuerPrincipal(cert).getEncoded());
        }
        catch (IOException e)
        {
            ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.crlIssuerException");
            throw new CertPathReviewerException(msg,e);
        }
    
        crlselect.setCertificateChecking(cert);
    
        Iterator crl_iter;
        try 
        {
            Collection crl_coll = findCRLs(crlselect, paramsPKIX.getCertStores());
            crl_iter = crl_coll.iterator();
            
            if (crl_coll.isEmpty())
            {
                // notifcation - no local crls found
                crl_coll = findCRLs(new X509CRLSelector(),paramsPKIX.getCertStores());
                Iterator it = crl_coll.iterator();
                List nonMatchingCrlNames = new ArrayList();
                while (it.hasNext())
                {
                    nonMatchingCrlNames.add(((X509CRL) it.next()).getIssuerX500Principal());
                }
                int numbOfCrls = nonMatchingCrlNames.size();
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,
                        "CertPathReviewer.noCrlInCertstore",
                        new Object[] {new UntrustedInput(crlselect.getIssuers()),
                                      new UntrustedInput(nonMatchingCrlNames),
                                      new Integer(numbOfCrls)});
                addNotification(msg,index);
            }

        }
        catch (AnnotatedException ae)
        {
            ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.crlExtractionError",
                    new Object[] {ae.getCause().getMessage(),ae.getCause()});
            addError(msg,index);
            crl_iter = new ArrayList().iterator();
        }
        boolean validCrlFound = false;
        X509CRL crl = null;
        while (crl_iter.hasNext())
        {
            crl = (X509CRL)crl_iter.next();
            
            if (crl.getNextUpdate() == null
                || new Date().before(crl.getNextUpdate()))
            {
                validCrlFound = true;
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,
                        "CertPathReviewer.localValidCRL",
                        new Object[] {crl.getThisUpdate(),crl.getNextUpdate()});
                addNotification(msg,index);
                break;
            }
            else
            {
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,
                        "CertPathReviewer.localInvalidCRL",
                        new Object[] {crl.getThisUpdate(),crl.getNextUpdate()});
                addNotification(msg,index);
            }
        }
        
        // if no valid crl was found in the CertStores try to get one from a
        // crl distribution point
        if (!validCrlFound)
        {
            X509CRL onlineCRL = null;
            Iterator urlIt = crlDistPointUrls.iterator();
            while (urlIt.hasNext())
            {
                try
                {
                    String location = (String) urlIt.next();
                    onlineCRL = getCRL(location);
                    if (onlineCRL != null)
                    {
                        if (onlineCRL.getNextUpdate() == null
                            || new Date().before(onlineCRL.getNextUpdate()))
                        {
                            validCrlFound = true;
                            ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,
                                    "CertPathReviewer.onlineValidCRL",
                                    new Object[] {onlineCRL.getThisUpdate(),
                                                  onlineCRL.getNextUpdate(),
                                                  new UntrustedInput(location)});
                            addNotification(msg,index);
                            crl = onlineCRL;
                            break;
                        }
                        else
                        {
                            ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,
                                    "CertPathReviewer.onlineInvalidCRL",
                                    new Object[] {onlineCRL.getThisUpdate(),
                                                  onlineCRL.getNextUpdate(),
                                                  new UntrustedInput(location)});
                            addNotification(msg,index);
                        }
                    }
                }
                catch (CertPathReviewerException cpre)
                {
                    addNotification(cpre.getErrorMessage(),index);
                }
            }
        }
        
        // check the crl
        X509CRLEntry crl_entry;
        if (crl != null)
        {
            if (sign != null)
            {
                boolean[] keyusage = sign.getKeyUsage();

                if (keyusage != null
                    && (keyusage.length < 7 || !keyusage[CRL_SIGN]))
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.noCrlSigningPermited");
                    throw new CertPathReviewerException(msg);
                }
            }

            if (workingPublicKey != null)
            {
                try
                {
                    crl.verify(workingPublicKey, "BC");
                }
                catch (Exception e)
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.crlVerifyFailed");
                    throw new CertPathReviewerException(msg,e);
                }
            }
            else // issuer public key not known
            {
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.crlNoIssuerPublicKey");
                throw new CertPathReviewerException(msg);
            }

            crl_entry = crl.getRevokedCertificate(cert.getSerialNumber());
            if (crl_entry != null)
            {
                String reason = null;
                
                if (crl_entry.hasExtensions())
                {
                    DEREnumerated reasonCode;
                    try
                    {
                        reasonCode = DEREnumerated.getInstance(getExtensionValue(crl_entry, X509Extensions.ReasonCode.getId()));
                    }
                    catch (AnnotatedException ae)
                    {
                        ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.crlReasonExtError");
                        throw new CertPathReviewerException(msg,ae);
                    }
                    if (reasonCode != null)
                    {
                        reason = crlReasons[reasonCode.getValue().intValue()];
                    }
                }
                
                // FIXME reason not i18n
                
                if (!validDate.before(crl_entry.getRevocationDate()))
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.certRevoked",
                            new Object[] {crl_entry.getRevocationDate(),reason});
                    throw new CertPathReviewerException(msg);
                }
                else // cert was revoked after validation date
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.revokedAfterValidation",
                            new Object[] {crl_entry.getRevocationDate(),reason});
                    addNotification(msg,index);
                }
            }
            else // cert is not revoked
            {
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.notRevoked");
                addNotification(msg,index);
            }
            
            //
            // warn if a new crl is available
            //
            if (crl.getNextUpdate() != null && crl.getNextUpdate().before(new Date()))
            {
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.crlUpdateAvailable",
                        new Object[] {crl.getNextUpdate()});
                addNotification(msg,index);
            }
            
            //
            // check the DeltaCRL indicator, base point and the issuing distribution point
            //
            DERObject idp;
            try
            {
                idp = getExtensionValue(crl, ISSUING_DISTRIBUTION_POINT);
            }
            catch (AnnotatedException ae)
            {
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.distrPtExtError");
                throw new CertPathReviewerException(msg);
            }
            DERObject dci;
            try
            {
                dci = getExtensionValue(crl, DELTA_CRL_INDICATOR);
            }
            catch (AnnotatedException ae)
            {
                ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.deltaCrlExtError");
                throw new CertPathReviewerException(msg);
            }

            if (dci != null)
            {
                X509CRLSelector baseSelect = new X509CRLSelector();

                try
                {
                    baseSelect.addIssuerName(getIssuerPrincipal(crl).getEncoded());
                }
                catch (IOException e)
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.crlIssuerException");
                    throw new CertPathReviewerException(msg,e);
                }

                baseSelect.setMinCRLNumber(((DERInteger)dci).getPositiveValue());
                try
                {
                    baseSelect.setMaxCRLNumber(((DERInteger)getExtensionValue(crl, CRL_NUMBER)).getPositiveValue().subtract(BigInteger.valueOf(1)));
                }
                catch (AnnotatedException ae)
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.crlNbrExtError");
                    throw new CertPathReviewerException(msg,ae);
                }
                
                boolean  foundBase = false;
                Iterator it;
                try 
                {
                    it  = findCRLs(baseSelect, paramsPKIX.getCertStores()).iterator();
                }
                catch (AnnotatedException ae)
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.crlExtractionError");
                    throw new CertPathReviewerException(msg,ae);
                }
                while (it.hasNext())
                {
                    X509CRL base = (X509CRL)it.next();

                    DERObject baseIdp;
                    try
                    {
                        baseIdp = getExtensionValue(base, ISSUING_DISTRIBUTION_POINT);
                    }
                    catch (AnnotatedException ae)
                    {
                        ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.distrPtExtError");
                        throw new CertPathReviewerException(msg,ae);
                    }
                    
                    if (idp == null)
                    {
                        if (baseIdp == null)
                        {
                            foundBase = true;
                            break;
                        }
                    }
                    else
                    {
                        if (idp.equals(baseIdp))
                        {
                            foundBase = true;
                            break;
                        }
                    }
                }
                
                if (!foundBase)
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.noBaseCRL");
                    throw new CertPathReviewerException(msg);
                }
            }

            if (idp != null)
            {
                IssuingDistributionPoint    p = IssuingDistributionPoint.getInstance(idp);
                BasicConstraints bc = null;
                try
                {
                    bc = BasicConstraints.getInstance(getExtensionValue(cert, BASIC_CONSTRAINTS));
                }
                catch (AnnotatedException ae)
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.crlBCExtError");
                    throw new CertPathReviewerException(msg,ae);
                }
                
                if (p.onlyContainsUserCerts() && (bc != null && bc.isCA()))
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.crlOnlyUserCert");
                    throw new CertPathReviewerException(msg);
                }
                
                if (p.onlyContainsCACerts() && (bc == null || !bc.isCA()))
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.crlOnlyCaCert");
                    throw new CertPathReviewerException(msg);
                }
                
                if (p.onlyContainsAttributeCerts())
                {
                    ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.crlOnlyAttrCert");
                    throw new CertPathReviewerException(msg);
                }
            }
        }
        
        if (!validCrlFound)
        {
            ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.noValidCrlFound");
            throw new CertPathReviewerException(msg);
        }
    
    }
    
    private Vector getCRLDistUrls(CRLDistPoint crlDistPoints, AuthorityInformationAccess authInfoAcc)
    {
        Vector urls = new Vector();
        
        if (crlDistPoints != null)
        {
            DistributionPoint[] distPoints = crlDistPoints.getDistributionPoints();
            for (int i = 0; i < distPoints.length; i++)
            {
                DistributionPointName dp_name = distPoints[i].getDistributionPoint();
                if (dp_name.getType() == DistributionPointName.FULL_NAME)
                {
                    GeneralName[] generalNames = GeneralNames.getInstance(dp_name.getName()).getNames();
                    for (int j = 0; j < generalNames.length; j++)
                    {
                        if (generalNames[j].getTagNo() == GeneralName.uniformResourceIdentifier)
                        {
                            String url = ((DERIA5String) generalNames[j].getName()).getString();
                            urls.add(url);
                        }
                    }
                }
            }
        }
        
        if (authInfoAcc != null)
        {
            AccessDescription[] ads = authInfoAcc.getAccessDescriptions();
            for (int i = 0; i < ads.length; i++)
            {
                if (ads[i].getAccessMethod().equals(AccessDescription.id_ad_caIssuers))
                {
                    GeneralName name = ads[i].getAccessLocation();
                    if (name.getTagNo() ==  GeneralName.uniformResourceIdentifier)
                    {
                        String url = ((DERIA5String) name.getName()).getString();
                        urls.add(url);
                    }
                }
            }
        }
        
        return urls;
    }
    
    private Vector getOCSPUrls(AuthorityInformationAccess authInfoAccess)
    {
        Vector urls = new Vector();
        
        if (authInfoAccess != null)
        {
            AccessDescription[] ads = authInfoAccess.getAccessDescriptions();
            for (int i = 0; i < ads.length; i++)
            {
                if (ads[i].getAccessMethod().equals(AccessDescription.id_ad_ocsp))
                {
                    GeneralName name = ads[i].getAccessLocation();
                    if (name.getTagNo() == GeneralName.uniformResourceIdentifier)
                    {
                        String url = ((DERIA5String) name.getName()).getString();
                        urls.add(url);
                    }
                }
            }
        }
        
        return urls;
    }
    
    private X509CRL getCRL(String location) throws CertPathReviewerException
    {
        X509CRL result = null;
        try
        {
            URL url = new URL(location);
            
            if (url.getProtocol().equals("http") || url.getProtocol().equals("https"))
            {
                HttpURLConnection conn = (HttpURLConnection) url.openConnection();
                conn.setUseCaches(false);
                conn.setConnectTimeout(2000);
                conn.setDoInput(true);
                conn.connect();
                if (conn.getResponseCode() == HttpURLConnection.HTTP_OK)
                {
                    CertificateFactory cf = CertificateFactory.getInstance("X.509","BC");
                    result = (X509CRL) cf.generateCRL(conn.getInputStream());
                }
                else
                {
                    throw new Exception(conn.getResponseMessage());
                }
            }
        }
        catch (Exception e)
        {
            ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,
                    "CertPathReviewer.loadCrlDistPointError",
                    new Object[] {new UntrustedInput(location),
                                  e.getMessage(),e});
            throw new CertPathReviewerException(msg);
        }
        return result;
    }
    
    private Collection getTrustAnchors(X509Certificate cert, Set trustanchors) throws CertPathReviewerException
    {
        Collection trustColl = new ArrayList();
        Iterator it = trustanchors.iterator();
        
        X509CertSelector certSelectX509 = new X509CertSelector();

        try
        {
            certSelectX509.setSubject(getEncodedIssuerPrincipal(cert).getEncoded());
        }
        catch (IOException ex)
        {
            ErrorBundle msg = new ErrorBundle(RESOURCE_NAME,"CertPathReviewer.trustAnchorIssuerError");
            throw new CertPathReviewerException(msg);
        }

        while (it.hasNext())
        {
            TrustAnchor trust = (TrustAnchor) it.next();
            if (trust.getTrustedCert() != null)
            {
                if (certSelectX509.match(trust.getTrustedCert()))
                {
                    trustColl.add(trust);
                }
            }
            else if (trust.getCAName() != null && trust.getCAPublicKey() != null)
            {
                X500Principal certIssuer = getEncodedIssuerPrincipal(cert);
                X500Principal caName = new X500Principal(trust.getCAName());
                if (certIssuer.equals(caName))
                {
                    trustColl.add(trust);
                }
            }
        }
        return trustColl;
    }
}
