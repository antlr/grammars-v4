DECLARE @CurrentEmployee hierarchyid
SELECT @CurrentEmployee = OrgNode FROM HumanResources.EmployeeDemo
WHERE LoginID = 'adventure-works\david0'

SELECT OrgNode.ToString() AS Text_OrgNode, *
FROM HumanResources.EmployeeDemo
WHERE OrgNode.GetAncestor(1) = @CurrentEmployee ;

DECLARE @CurrentEmployee hierarchyid
SELECT @CurrentEmployee = OrgNode FROM HumanResources.EmployeeDemo
WHERE LoginID = 'adventure-works\ken0'

SELECT OrgNode.ToString() AS Text_OrgNode, *
FROM HumanResources.EmployeeDemo
WHERE OrgNode.GetAncestor(2) = @CurrentEmployee ;

DECLARE @CurrentEmployee hierarchyid
SELECT @CurrentEmployee = OrgNode FROM HumanResources.EmployeeDemo
WHERE LoginID = 'adventure-works\david0'

SELECT OrgNode.ToString() AS Text_OrgNode, *
FROM HumanResources.EmployeeDemo
WHERE OrgNode.GetAncestor(0) = @CurrentEmployee ;

DECLARE @CurrentEmployee hierarchyid ;
DECLARE @TargetEmployee hierarchyid ;
SELECT @CurrentEmployee = '/2/3/1.2/5/3/' ;
SELECT @TargetEmployee = @CurrentEmployee.GetAncestor(2) ;
SELECT @TargetEmployee.ToString(), @TargetEmployee ;

DECLARE @Manager hierarchyid;
SET @Manager = CAST('/3/1/' AS hierarchyid);

INSERT HumanResources.EmployeeDemo (OrgNode, LoginID, Title, HireDate)
VALUES
(@Manager.GetDescendant(NULL, NULL),
'adventure-works\FirstNewEmployee', 'Application Intern', '3/11/07') ;

DECLARE @Manager hierarchyid, @Child1 hierarchyid;

SET @Manager = CAST('/3/1/' AS hierarchyid);
SET @Child1 = CAST('/3/1/1/' AS hierarchyid);

INSERT HumanResources.EmployeeDemo (OrgNode, LoginID, Title, HireDate)
VALUES
(@Manager.GetDescendant(@Child1, NULL),
'adventure-works\SecondNewEmployee', 'Application Intern', '3/11/07') ;

DECLARE @Manager hierarchyid, @Child1 hierarchyid, @Child2 hierarchyid;

SET @Manager = CAST('/3/1/' AS hierarchyid);
SET @Child1 = CAST('/3/1/1/' AS hierarchyid);
SET @Child2 = CAST('/3/1/2/' AS hierarchyid);

INSERT HumanResources.EmployeeDemo (OrgNode, LoginID, Title, HireDate)
VALUES
(@Manager.GetDescendant(@Child1, @Child2),
'adventure-works\ThirdNewEmployee', 'Application Intern', '3/11/07') ;

DECLARE @h hierarchyid = hierarchyid::GetRoot();
DECLARE @c hierarchyid = @h.GetDescendant(NULL, NULL);
SELECT @c.ToString();
DECLARE @c2 hierarchyid = @h.GetDescendant(@c, NULL);
SELECT @c2.ToString();
SET @c2 = @h.GetDescendant(@c, @c2);
SELECT @c2.ToString();
SET @c = @h.GetDescendant(@c, @c2);
SELECT @c.ToString();
SET @c2 = @h.GetDescendant(@c, @c2);
SELECT @c2.ToString();

SELECT OrgNode.ToString() AS Text_OrgNode,
OrgNode.GetLevel() AS EmpLevel, *
FROM HumanResources.EmployeeDemo;

SELECT OrgNode.ToString() AS Text_OrgNode,
OrgNode.GetLevel() AS EmpLevel, *
FROM HumanResources.EmployeeDemo
WHERE OrgNode.GetLevel() = 2;

SELECT OrgNode.ToString() AS Text_OrgNode,
OrgNode.GetLevel() AS EmpLevel, *
FROM HumanResources.EmployeeDemo
WHERE OrgNode.GetLevel() = 0;

SELECT OrgNode.ToString() AS Text_OrgNode, *
FROM HumanResources.EmployeeDemo
WHERE OrgNode = hierarchyid::GetRoot()

DECLARE @Manager hierarchyid
SELECT @Manager = OrgNode FROM HumanResources.EmployeeDemo
  WHERE LoginID = 'adventure-works\dylan0'

SELECT * FROM HumanResources.EmployeeDemo
WHERE OrgNode.IsDescendantOf(@Manager) = 1

DECLARE @Manager hierarchyid, @Employee hierarchyid, @LoginID nvarchar(256)
SELECT @Manager = OrgNode FROM HumanResources.EmployeeDemo
WHERE LoginID = 'adventure-works\terri0' ;

SELECT @Employee = OrgNode, @LoginID = LoginID FROM HumanResources.EmployeeDemo
  WHERE LoginID = 'adventure-works\rob0'

IF @Employee.IsDescendantOf(@Manager) = 1
   BEGIN
    PRINT 'LoginID ' + @LoginID + ' is a subordinate of the selected Manager.'
   END
ELSE
   BEGIN
    PRINT 'LoginID ' + @LoginID + ' is not a subordinate of the selected Manager.' ;
   END

DECLARE @StringValue AS NVARCHAR(4000), @hierarchyidValue AS hierarchyid
SET @StringValue = '/1/1/3/'
SET @hierarchyidValue = 0x5ADE

SELECT hierarchyid::Parse(@StringValue) AS hierarchyidRepresentation,
@hierarchyidValue.ToString() AS StringRepresentation ;
GO

DECLARE @SubjectEmployee hierarchyid , @OldParent hierarchyid, @NewParent hierarchyid
SELECT @SubjectEmployee = OrgNode FROM HumanResources.EmployeeDemo
  WHERE LoginID = 'adventure-works\gail0' ;
SELECT @OldParent = OrgNode FROM HumanResources.EmployeeDemo
  WHERE LoginID = 'adventure-works\roberto0' ; -- who is /1/1/
SELECT @NewParent = OrgNode FROM HumanResources.EmployeeDemo
  WHERE LoginID = 'adventure-works\wanida0' ; -- who is /2/3/

SELECT OrgNode.ToString() AS Current_OrgNode_AS_Text,
(@SubjectEmployee. GetReparentedValue(@OldParent, @NewParent) ).ToString() AS Proposed_OrgNode_AS_Text,
OrgNode AS Current_OrgNode,
@SubjectEmployee. GetReparentedValue(@OldParent, @NewParent) AS Proposed_OrgNode,
*
FROM HumanResources.EmployeeDemo
WHERE OrgNode = @SubjectEmployee ;
GO

DECLARE @SubjectEmployee hierarchyid , @OldParent hierarchyid, @NewParent hierarchyid
SELECT @SubjectEmployee = OrgNode FROM HumanResources.EmployeeDemo
  WHERE LoginID = 'adventure-works\gail0' ; -- Node /1/1/2/
SELECT @OldParent = OrgNode FROM HumanResources.EmployeeDemo
  WHERE LoginID = 'adventure-works\roberto0' ; -- Node /1/1/
SELECT @NewParent = OrgNode FROM HumanResources.EmployeeDemo
  WHERE LoginID = 'adventure-works\wanida0' ; -- Node /2/3/

UPDATE HumanResources.EmployeeDemo
SET OrgNode = @SubjectEmployee. GetReparentedValue(@OldParent, @NewParent)
WHERE OrgNode = @SubjectEmployee ;

SELECT OrgNode.ToString() AS Current_OrgNode_AS_Text,
*
FROM HumanResources.EmployeeDemo
WHERE LoginID = 'adventure-works\gail0' ; -- Now node /2/3/2/

SELECT OrgNode,
OrgNode.ToString() AS Node
FROM HumanResources.EmployeeDemo
ORDER BY OrgNode ;
GO

DECLARE @StringValue AS nvarchar(4000), @hierarchyidValue AS hierarchyid
SET @StringValue = '/1/1/3/'
SET @hierarchyidValue = 0x5ADE

SELECT hierarchyid::Parse(@StringValue) AS hierarchyidRepresentation,
@hierarchyidValue.ToString() AS StringRepresentation ;
GO
