//
//  FSBaseViewController.m
//  FlickStackr
//
//  Created by Carlos Mejia on 2013-05-01.
//  Copyright (c) 2013 iPont. All rights reserved.
//

#import "FSBaseViewController.h"
#import "IPEnv.h"
#import "FPAppIpadDelegate.h"
#import "FPAppIphoneDelegate.h"
#import "FSPreferences.h"
#import "PhotoViewController.h"
#import "DetailsViewController.h"
#import "PhotoThumbsViewController.h"
#import "ApplicationState.h"
#import "UserViewController.h"
#import "ProviderSourceAsPhoto.h"
#import "AppSpecific.h"
#import "ToolbarNoBack.h"
#import "DragRefreshView.h"
#import "PhotoCell.h"
#import "IPToolbarButton.h"
#import "FSThemeManager.h"
#import "IPBlurView.h"
#import "FSWindowManager.h"
#import "GenericUIKitWindowController.h"

@interface FSBaseViewController ()
{
    CGFloat lastScrollPos;
    BOOL scrollDraggingForHideBar;
    BOOL hidingNavBar;
    BOOL processingHideShowBar;
    BOOL hideNavBarPref;
    BOOL presentingSort;
    BOOL secondTimeAppear;
    NSArray *iPhoneSecondBarButtons;
    GenericUIKitWindowController *macWindowController;
    IPActionSheet *sortOptions;
}
@end

@implementation FSBaseViewController
@synthesize session;
@synthesize viewSituation;
@synthesize showPopupOnDisplay;
@synthesize refreshHeaderView;
@synthesize loadingLabel;
@synthesize toolbarPressedLabel;
@synthesize windowController;
@synthesize currentToolbarButtons;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

-(instancetype)init
{
    if (self = [super initWithNibName:nil bundle:nil])
    {
        windowController = [[FSWindowManager instance] mostRecent];  // iOS !!
    }
    return self;
}

-(instancetype)initWithFSWindowController:(FSWindowController*)fsWindowController
{
    if (self = [super initWithNibName:nil bundle:nil])
    {
        windowController = fsWindowController;  // iOS !!
    }
    return self;
}

-(instancetype)initWithNibName:(NSString *)nibNameOrNil windowController:(FSWindowController*)fsWindowController
{
    if (self = [super initWithNibName:nibNameOrNil bundle:nil])
    {
        windowController = fsWindowController;
    }
    return self;
}

-(instancetype)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    if (self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil])
    {
        windowController = [FSWindowManager instance].mostRecent;  // iOS !!
    }
    return self;
}


-(instancetype)initWithCoder:(NSCoder *)coder
{
    if (self = [super initWithCoder:coder])
    {
        windowController = [FSWindowManager instance].mostRecent;  // iOS !!
    }
    return self;
}

- (ViewSituation)viewSituation
{
	if (viewSituation==VSUNKNOWN)
	{
		viewSituation = VSALONE;
        if ([IPEnv isIpad])
            viewSituation = [FPAppIpadDelegate calculateViewSituation:self];
	}
	return viewSituation;
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

-(UIView*)viewForQuickNotification
{
    return self.view;
}
-(BOOL)showTitleInNavigationBar
{
    return YES;
}
-(BOOL)showTitleInTableHeader
{
    return YES;
}

-(BOOL)forceIPadDropDown
{
    return YES;
}

-(void)reloadTable
{
}

-(void)clearSort
{
}

-(void)prepareToClose
{
}

-(void)clearToolbar
{
}

-(void)displayTip:(NSString*)message
{
}

-(IBAction)toggleSelection:(id)sender
{
}

-(IBAction)deleteSelected:(id)sender
{
}

-(IBAction)modifySelected:(id)sender
{
}

-(void)setIphoneBackButton
{
	if ([IPEnv isPhone])
    {
        NSString *backText = [IPEnv isOs7] ? @" " : @"<";
		self.navigationItem.backBarButtonItem = [[UIBarButtonItem alloc] initWithTitle:backText style:UIBarButtonItemStyleBordered target:nil action:nil];
    }
}

-(void)setIphoneTopTitle:(FSBaseViewController*)newController
{
	if ([IPEnv isPhone])
    {
        if (!newController.showTitleInNavigationBar)
            self.navigationController.navigationBar.topItem.title = @"";
    }
}

- (void)willRotateToInterfaceOrientation:(UIInterfaceOrientation)toInterfaceOrientation duration:(NSTimeInterval)duration
{
    [super willRotateToInterfaceOrientation:toInterfaceOrientation duration:duration];
    if ([IPEnv isIpad] && UIInterfaceOrientationIsLandscape(toInterfaceOrientation))
        [self showNavigationBar];
    scrollDraggingForHideBar = NO;
}
- (void)willAnimateRotationToInterfaceOrientation:(UIInterfaceOrientation)toInterfaceOrientation duration:(NSTimeInterval)duration
{
    [super willAnimateRotationToInterfaceOrientation:toInterfaceOrientation duration:duration];
    if (self.toolbarPressedLabel)
        [self.toolbarPressedLabel handleRotate];
}
-(BOOL)shouldHideBackButton
{
    BOOL isPortrait = (UIInterfaceOrientationIsPortrait([[UIApplication sharedApplication] statusBarOrientation]));
    return (([FSPreferences instance].iPadPortraitWithNoSplitView && isPortrait) ? NO : YES);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// View controller navigation

-(void)viewDidLoadMac
{
    // do nothing at this level
}

-(void)viewResizedMac
{
    // do nothing at this level
}

-(void)viewDidLoad
{
    [super viewDidLoad];
    if ([IPEnv isMac])
    {
        [self viewDidLoadMac];
        self.view.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight;
    }
    hideNavBarPref = [FSPreferences instance].autoHideNavigationBar;
}

-(void)pushViewControllerOnRightWithNewController:(FSBaseViewController*)newController atTop:(NSInteger)atTop  dismissPopover:(BOOL)shouldDismissPopover animated:(BOOL)animated
{
	if (self.viewSituation == VSLEFT)
	{
		UIViewController *old = [self.windowController rightViewController];
        
        if ([old conformsToProtocol:@protocol(FSViewController)])
        {
            [(id<FSViewController>)old prepareToClose];
        }
        
		newController.navigationItem.hidesBackButton = YES;
		newController.navigationItem.leftBarButtonItem = (self.shouldHideBackButton) ? old.navigationItem.leftBarButtonItem : nil;
		newController.navigationItem.leftBarButtonItem.title = [self.windowController leftViewController].title;
		
		UINavigationController *sibling = self.windowController.rightNavigationController;
		if (atTop)
		{
			[sibling popToRootViewControllerAnimated:NO];
			if (atTop==1 && animated==YES)
				[sibling pushViewController:newController animatedTransition:UIViewAnimationOptionTransitionCurlDown];
			else
                
				[sibling pushViewController:newController animated:NO];
		}
		else
		{
			[sibling popViewControllerAnimated:NO];
			[sibling pushViewController:newController animated:NO];
		}
		
		if (shouldDismissPopover)
			[self.windowController dismissMainPopover];
		
	}
	else
	{
		[self setIphoneBackButton];
		[self.navigationController pushViewController:newController animated:animated];
        [self setIphoneTopTitle:newController];
	}
}

-(UITableView*)tableView
{
    return nil;
}

- (UIScrollView *)scrollView
{
    UITableView *tabView = self.tableView;
    if (tabView)
        return tabView;
    
    return SAFE_CAST(super.view, UIScrollView);
}

-(void)copyPosition:(FSBaseViewController*)other selectedRow:(NSInteger)selRow selectedSection:(NSInteger)selSection
{
    
}
- (void)setSelectedRowAtAppear:(BOOL)forceScroll
{
    
}
-(void)scrollBeforeMove
{
    
}
- (void)pushViewControllerFromRightToLeftWithCloned:(FSBaseViewController*)clonedController newRight:(FSBaseViewController*)completelyNewController  dismissPopover:(BOOL)shouldDismissPopover animated:(BOOL)animated
{
    @try
    {
        [self scrollBeforeMove];
        if (self.viewSituation == VSLEFT)
        {
            FSBaseViewController *current = SAFE_CAST(self.windowController.rightNavigationController.topViewController, FSBaseViewController);
            [current showNavigationBar];

            [self pushViewControllerOnRightWithNewController:completelyNewController atTop:NO  dismissPopover:shouldDismissPopover animated:animated];
            return;
        }
        
        if (self.viewSituation== VSRIGHT)
        {
            completelyNewController.navigationItem.hidesBackButton = self.shouldHideBackButton;
            UIViewController*old = [self.windowController rightViewController];
            completelyNewController.navigationItem.leftBarButtonItem = (self.shouldHideBackButton) ? old.navigationItem.leftBarButtonItem : nil;
            completelyNewController.navigationItem.leftBarButtonItem.title = old.title;
        }
        
        
        [self setIphoneBackButton];
        [self.navigationController pushViewController:completelyNewController animated:animated];
        [self setIphoneTopTitle:completelyNewController];
        
        if (self.viewSituation == VSRIGHT)
        {
            UINavigationController *leftSibling = self.windowController.leftNavigationController;
            
            if ([leftSibling.topViewController conformsToProtocol:@protocol(LeftFSViewController)] )
            {
                ((id<LeftFSViewController>)leftSibling.topViewController).disappeared = YES;
            }
			
            id<FSPhotoSetViewController> mePhotoSet = [self conformsToProtocol:@protocol(FSPhotoSetViewController)] ? (id<FSPhotoSetViewController>)self : nil;
            
            if (mePhotoSet)
            {
                NSInteger selectedRow = -1;
                NSInteger selectedSection = 0;
                if ([self.tableView isKindOfClass:[UITableView class]])
                {
                    NSIndexPath *selected = self.tableView.indexPathForSelectedRow;
                    selectedRow = (selected) ? selected.row : -1;
                    selectedSection = (selected) ? selected.section : 0;
                }
                
                [clonedController copyPosition:self selectedRow:selectedRow selectedSection:selectedSection];
                
                [leftSibling pushViewController:clonedController animated:animated];
                if ([clonedController respondsToSelector:@selector(setSelectedRowAtAppear:)])
                {
                    [clonedController setSelectedRowAtAppear:YES];
                }
            }
            else
            {
                [leftSibling pushViewController:clonedController animated:animated];
            }
        }
    }
    @catch (NSException *exception)
    {
        NSLog(@"EX(pushViewControllerFromRightToLeft) %@",exception);
        
    }
}


-(void)pushViewControllerFromRightToLeft:(FSBaseViewController *)completelyNewController
{
    FSBaseViewController *selfClone;
    if (viewSituation == VSRIGHT)
        selfClone = [self cloneWithViewSituation:VSLEFT];
    [self pushViewControllerFromRightToLeft:selfClone newRight:completelyNewController];
}

- (void)pushViewControllerOnRight:(UIViewController*)newController atTop:(NSInteger)atTop dismissPopover:(BOOL)shouldDismissPopover
{
	[self pushViewControllerOnRightWithNewController:newController atTop:atTop dismissPopover:shouldDismissPopover animated:YES];
}

- (void)pushViewControllerFromRightToLeft:(FSBaseViewController*)clonedController newRight:(UIViewController*)completelyNewController
{
	[self pushViewControllerFromRightToLeftWithCloned:clonedController newRight:completelyNewController dismissPopover:NO animated:YES];
}

- (void)pushViewControllerFromRightToLeft:(FSBaseViewController*)clonedController newRight:(UIViewController*)completelyNewController dismissPopover:(BOOL)shouldDismissPopover
{
	[self pushViewControllerFromRightToLeftWithCloned:clonedController newRight:completelyNewController dismissPopover:shouldDismissPopover animated:YES];
}


-(void)removeControllerFromStack:(FSBaseViewController*)toRemove
{
    UINavigationController* navigationCont = self.navigationController;
	NSArray *exi = navigationCont.viewControllers;
	NSMutableArray * ns2 = [NSMutableArray arrayWithCapacity:[exi count]];
	for (NSUInteger i=0;i<[exi count];i++)
	{
		id item=exi[i];
		if (toRemove != item)
			[ns2 addObject:item];
	}
    if (ns2.count != exi.count)
        [ navigationCont setViewControllers:ns2 animated:NO];
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Create standard view controllers
//

-(FSBaseViewController*)createPreferredPhotoListView:(ProviderPhotoSource*)inPhotos
{
    return [self createPreferredPhotoListViewForCurrentView:self photos:inPhotos];
}
-(FSBaseViewController*)createPreferredPhotoListViewForCurrentView:(id<FSViewController>)currentView photos:(ProviderPhotoSource*)inPhotos
{
    @try
    {
        Class viewControllerClass = inPhotos.preferredControllerClass;
        
        if (viewControllerClass)
        {
            return [[viewControllerClass alloc] initWithWindowController:self.windowController photos:inPhotos];
        }
        else
        {
            
            if ([FSPreferences instance].preferThumbView && inPhotos.containsPhotos)
            {
                return [[inPhotos.photoThumbsClass alloc] initWithWindowController:self.windowController photos:inPhotos situation:(currentView.viewSituation==VSALONE) ? VSALONE : VSRIGHT];
            }
            else
            {
                return [[inPhotos.photoListClass alloc] initWithWindowController:self.windowController photos:inPhotos];
            }
        }
    }
    @catch (NSException *exception)
    {
        NSLog(@"EX(CreatePrefferedList) %@",exception);
    }
    return nil;
}

-(FSBaseViewController*)createPreferredPhotoViewForPhoto:(ProviderPhoto*)inPhoto viewSituation:(ViewSituation)inViewSituation forcePhotoView:(BOOL)forcePhotoView;
{
	if (![FSPreferences instance].preferCommentsView || forcePhotoView)
	{
		return [[PhotoViewController alloc] initWithPhoto:inPhoto windowController:self.windowController situation:inViewSituation];
	}
	else
	{
		return [[DetailsViewController alloc] initWithWindowController:self.windowController photo:(ProviderPhoto*)inPhoto];
	}
}

-(FSBaseViewController*)createPhotoViewFromClassName:(NSString*)className  photo:(ProviderPhoto*)inPhoto viewSituation:(ViewSituation)inViewSituation 
{
	if (NSClassFromString(className) == [DetailsViewController class])
	{
		return [[DetailsViewController alloc] initWithWindowController:self.windowController photo:(ProviderPhoto*)inPhoto];
	}
	else
	{
		return [[PhotoViewController alloc] initWithPhoto:inPhoto windowController:self.windowController situation:inViewSituation];
	}
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

-(FSBaseViewController*)movePhotoDetailViewToPhoto:(ProviderPhoto*)fp forcePhotoView:(BOOL)forcePhotoView showOwner:(BOOL)showOwner
{
    FSBaseViewController *newMainController=nil;
    @try
    {
        if (!self.session.supportsPhotoDetails)
            forcePhotoView = YES;
        
        if (fp.isForbidden)
        {
            [self.windowController showLiteLimitation:@"LiteVersionMessage2"];
            return newMainController;
        }
        
        BOOL photoIsSet = fp.container;
        
        NSInteger index = fp.index;
        if (self.viewSituation==VSLEFT)
        {
            UIViewController *rightView = self.windowController.rightViewController;
            
            if (!showOwner && !photoIsSet && [rightView isKindOfClass:[PhotoViewController class]])
            {
                PhotoViewController *vc = (PhotoViewController*)rightView;
                newMainController = vc;
                ProviderPhoto* photoInPhotoView = vc.photos[fp.pid];
                if (!photoInPhotoView)
                {
                    photoInPhotoView = vc.photos[fp.index];
                    if (!photoInPhotoView)
                        photoInPhotoView = fp;
                }
                vc.centerPhoto = photoInPhotoView;
            }
            else if (!showOwner && !photoIsSet && [rightView isKindOfClass:[DetailsViewController class]])
            {
                DetailsViewController *cvc= (DetailsViewController*)rightView;
                newMainController = cvc;
                [cvc updatePhoto:fp];
                [self.windowController dismissMainPopover];
                
                // Handle the 'under' view
                UIViewController *previousVc = (self.windowController.rightNavigationController.viewControllers)[([self.navigationController.viewControllers count]-2)];
                if ([previousVc conformsToProtocol:@protocol(PhotoSetExtendedView)])
                {
                    [(id<PhotoSetExtendedView>)previousVc setSelectedRow:index section:0 scroll:YES];
                }
                
            }
            else
            {
                [self.windowController.state removeLastStep];  // it is a swap
                
                FSBaseViewController *anotherViewController=nil;
                if (showOwner)
                {
                    ProviderPhotoSource *userInfo = [self.session.photoSources source:PltRecentUser listId:fp.ownerId listName:fp.ownerName];
                    anotherViewController = [self createPreferredPhotoListViewForCurrentView:(id <FSViewController>) rightView photos:userInfo];
                }
                else if (photoIsSet)
                {
                    ProviderSourceAsPhoto *sourcePhoto = SAFE_CAST(fp, ProviderSourceAsPhoto);
                    if (sourcePhoto && sourcePhoto.sourceType == PltUserInfo)
                        sourcePhoto.sourceType = PltRecentUser;

                    ProviderPhotoSource *setSource = [fp recreatePhotoSource:self.session listType: (fp.providerPhotoSource.isForOffline) ? PltOffline : PltNone];
                    [sourcePhoto updateNumberOfItems:setSource];

                    anotherViewController = [self createPreferredPhotoListViewForCurrentView:(id <FSViewController>) rightView photos:setSource];
                    [self.windowController.state addStep:anotherViewController photoSource:setSource indexForPreviousStep:index];
                }
                else
                {
                    anotherViewController = [self createPreferredPhotoViewForPhoto:fp viewSituation:VSRIGHT forcePhotoView:forcePhotoView];
                    [self.windowController.state addStepPhoto:anotherViewController photoSource:fp.providerPhotoSource indexForThisAndPreviousStep:index];
                    
                }
                anotherViewController.navigationItem.hidesBackButton = self.shouldHideBackButton;
                newMainController = anotherViewController;
                
                anotherViewController.navigationItem.leftBarButtonItem = rightView.navigationItem.leftBarButtonItem;
                ((id<FSViewController>)anotherViewController).parentSwapView = rightView;
                [self.windowController.rightNavigationController pushViewController:anotherViewController animatedTransition:UIViewAnimationOptionTransitionNone];
                [self.windowController dismissMainPopover];
            }
        }
        else
        {
            FSBaseViewController *anotherViewController=nil;
            
            if (showOwner)
            {
                ProviderPhotoSource *userInfo = [self.session.photoSources source:PltRecentUser listId:fp.ownerId listName:fp.ownerName];
                anotherViewController = [self createPreferredPhotoListView:userInfo];
                [self.windowController.state addStep:anotherViewController photoSource:userInfo indexForPreviousStep:index];
            }
            else if (photoIsSet)
            {
                ProviderSourceAsPhoto *sourcePhoto = SAFE_CAST(fp, ProviderSourceAsPhoto);
                if (sourcePhoto && sourcePhoto.sourceType == PltUserInfo)
                    sourcePhoto.sourceType = PltRecentUser;

                ProviderPhotoSource *setSource = [fp recreatePhotoSource:self.session listType:(fp.providerPhotoSource.isForOffline) ? PltOffline : PltNone];
                [sourcePhoto updateNumberOfItems:setSource];
                
                anotherViewController = [self createPreferredPhotoListView:setSource];
                
                // Save state
                [self.windowController.state addStep:anotherViewController photoSource:setSource indexForPreviousStep:index];
            }
            else
            {
                anotherViewController = [self createPreferredPhotoViewForPhoto:fp viewSituation:self.viewSituation forcePhotoView:forcePhotoView];
                
                // Save state
                [self.windowController.state addStepPhoto:anotherViewController photoSource:fp.providerPhotoSource indexForThisAndPreviousStep:index];
            }
            
            
            anotherViewController.navigationItem.leftBarButtonItem = self.navigationItem.leftBarButtonItem;
            
            PhotoListViewController *newLeft=nil;
            if (self.viewSituation==VSRIGHT)
            {
                ProviderPhotoSource *photos = (ProviderPhotoSource*)fp.photoSource;
                newLeft = [[[self class] alloc] initWithWindowController:self.windowController photos:photos];
            }
            [self pushViewControllerFromRightToLeftWithCloned:newLeft newRight:anotherViewController dismissPopover:NO animated:YES];
            newMainController = anotherViewController;
            
        }
    }
    @catch (NSException *exception)
    {
        NSLog(@"EX(movePhotoDetailView) %@",exception);
    }
    
    return newMainController;
}

-(UIViewController*)movePhotoDetailViewToPhoto:(ProviderPhoto*)fp forcePhotoView:(BOOL)forcePhotoView
{
	return [self movePhotoDetailViewToPhoto:fp forcePhotoView:forcePhotoView showOwner:NO];
}

-(void)movePhotoListViews:(ProviderPhoto*)photo
{
	id<PhotoSetExtendedView> plvc=nil, undervc=nil;
	
	if (self.viewSituation==VSRIGHT)
	{
		if ([self.windowController.leftViewController conformsToProtocol:@protocol(PhotoSetExtendedView)])
		{
			plvc = (id<PhotoSetExtendedView>)self.windowController.leftViewController;
		}
	}
	
	if (self.viewSituation!=VSLEFT)
	{
		UIViewController *previousVc=nil;
		UINavigationController* navC= self.navigationController;
		
		if (!navC && ![IPEnv isIpad])
		{
			navC = self.windowController.navigationController;  // top controller
			previousVc = (navC.viewControllers)[(navC.viewControllers.count-1)];
		}
		else
		{
            if (navC)
            {
                if (navC.viewControllers.count >= 2)
                    previousVc = (navC.viewControllers)[(navC.viewControllers.count-2)];
                else
                {
                    NSLog(@"UNABLE to move photo list view");
                }
            }
		}
		
		if ([previousVc conformsToProtocol:@protocol(PhotoSetExtendedView)])
		{
			undervc = (id<PhotoSetExtendedView>)previousVc;
		}
		
	}
	// Save state (nils mean same source, just moved)
	[self.windowController.state addStepPhoto:self photoSource:photo.providerPhotoSource indexForThisAndPreviousStep:photo.index];
	
	if (plvc)
	{
        if (![plvc setSelectedPhotoById:photo.pid])
            [plvc setSelectedRow:photo.index section:0 scroll:YES];
	}
	if (undervc)
	{
        if (![undervc setSelectedPhotoById:photo.pid])
            [undervc setSelectedRow:photo.index section:0 scroll:YES];
	}
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

-(void)showQuickActionChooser:(UIGestureRecognizer *)recognizer doubleTap:(BOOL)doubleTap selected:(QuickActionChooserCompleteBlock)selected
{
    // show the best possible list based on the current service
    NSMutableArray *actions = [[NSMutableArray alloc] initWithCapacity:6];
    NSMutableArray *buttons = [[NSMutableArray alloc] initWithCapacity:6];
    if (session.supportsFavorites)
    {
        [buttons addObject:IPLS(@"QuickFavorite")];
        [actions addObject:@(QuickActionFavorite)];
    }
    if (session.supportsVotes)
    {
        [buttons addObject:IPLS(@"QuickVote")];
        [actions addObject:@(QuickActionVote)];
    }
    [buttons addObjectsFromArray:@[IPLS(@"QuickSaveLibrary"), IPLS(@"QuickSaveCloud"), IPLS(@"QuickStack")]];
    [actions addObjectsFromArray:@[ @(QuickActionSaveLibrary), @(QuickActionSaveCloud), @(QuickActionStack) ]];
    if (session.supportsPeopleBrowsing)
    {
        [buttons addObject:IPLS(@"QuickOwner")];
        [actions addObject:@(QuickActionOwner)];
    }
    
    NSString *title = (doubleTap) ? IPLS(@"QuickConfigDouble") : IPLS(@"QuickConfigLong");
    QuickActionChooserCompleteBlock selectedCopy = [selected copy];
    
    IPActionSheet *options = [[IPActionSheet alloc] initWithTitle:title destructiveButtonTitle:IPLS(@"QuickNone") otherButtonTitles:buttons];
    [options showInView:self.view sender:recognizer selected:^(NSInteger buttonIndex)
    {
        QuickAction newValue =  QuickActionOff;
        if (buttonIndex >= 0)
        {
            // button 0 is nothing, starting from 1 is the selection
            if (buttonIndex > 0)
            {
                newValue = [actions[buttonIndex-1] integerValue];
            }
            if (doubleTap)
                [FSPreferences instance].quickActionDoubleTap = newValue;
            else
                [FSPreferences instance].quickActionLongTap = newValue;
            if (selectedCopy)
                selectedCopy(newValue);
        }
    }];
}


-(void)setupNavigationBar
{
    self.navigationController.navigationBar.barStyle = [FSThemeManager navigationBarStyle];
    self.navigationController.navigationBar.translucent = [FSThemeManager navigationBarTranslucent];
    self.navigationController.navigationBar.tintColor =  [FSThemeManager toolbarButtonColor];
    self.navigationController.toolbar.tintColor = [FSThemeManager toolbarButtonColor];
}
-(NSArray*)setupQuickActionRecognizers
{
    return [self setupQuickActionRecognizersWithOtherView:nil];
}

-(NSArray*)setupQuickActionRecognizersWithOtherView:(UIView*)otherView
{
    FSPreferences *prefs = [FSPreferences instance];
    if (!otherView)
        otherView = self.view;
    NSMutableArray *allRecognizers = [[NSMutableArray alloc] initWithCapacity:2];
    
    if (prefs.quickActionDoubleTap != QuickActionOff)
    {
        UITapGestureRecognizer *doubleTapRecognizer = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(handleDoubleTap:)];
        doubleTapRecognizer.numberOfTapsRequired = 2;
        [doubleTapRecognizer setDelaysTouchesBegan:YES];
        [doubleTapRecognizer setCancelsTouchesInView:YES];
        [allRecognizers addObject:doubleTapRecognizer];
        [otherView addGestureRecognizer:doubleTapRecognizer];
    }
    
    if (prefs.quickActionLongTap != QuickActionOff)
    {
        UILongPressGestureRecognizer *longTapRecognizer = [[UILongPressGestureRecognizer alloc] initWithTarget:self action:@selector(handleLongTap:)];
        [otherView addGestureRecognizer:longTapRecognizer];
        [allRecognizers addObject:longTapRecognizer];
    }
    return allRecognizers;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Loading, refreshing,
//

-(CGFloat)statusBarHeight
{
    CGSize statBarSize = [[UIApplication sharedApplication] statusBarFrame].size;
    return MIN(statBarSize.height, statBarSize.width);
}
-(UIView*)createBlackSectionHeader
{
    return [self createBlackSectionHeader:self.title textHeight:0];
}
-(UIView*)createBlackSectionHeader:(NSString*)headerText textHeight:(CGFloat)textHeight 
{
    UIView *mainView;
    UILabel *header;

    IPBlurView *blurView = [[IPBlurView alloc] initWithFrame:CGRectMake(0, 0, 320, (textHeight) ? textHeight : 22)];
    mainView = blurView;
    CGRect labelSize = blurView.bounds;
    if (textHeight != 0)
        labelSize.size.height = textHeight;
    labelSize.origin.x += 6;
    labelSize.size.width -= 12;
    header = [[UILabel alloc] initWithFrame:labelSize];
    header.backgroundColor = [UIColor clearColor];
    header.autoresizingMask = UIViewAutoresizingFlexibleWidth |  ((textHeight==0) ? UIViewAutoresizingFlexibleHeight : 0);
    //blurView.blurTintColor = self.navigationController.navigationBar.barTintColor;
    //blurView.backgroundColor = [FSThemeManager photoListBarColor:self.showingThumbs];
    blurView.backgroundColor = [UIColor clearColor];
    [mainView addSubview:header];
    header.tag = 100;
	header.text = headerText;
    header.textColor = [FSThemeManager barTextColor];
	header.textAlignment = NSTextAlignmentCenter;
	header.font = [UIFont systemFontOfSize:13];
    header.numberOfLines = 2;
    mainView.autoresizingMask = UIViewAutoresizingFlexibleWidth;
	return mainView;
}

-(void)adjustNavigationBarForModal:(UINavigationBar*)navBar mainContent:(UIView*)mainContent mainScroll:(UIScrollView*)mainScroll  alwaysFullScreen:(BOOL)alwaysFullScreen
{
    if (![IPEnv isIpad])
        navBar.titleTextAttributes = @{UITextAttributeFont:[UIFont boldSystemFontOfSize:14]};

    navBar.barStyle = [FSThemeManager navigationBarStyle];
    navBar.translucent = [FSThemeManager navigationBarTranslucent];
    navBar.tintColor =  [FSThemeManager toolbarButtonColor];

    if ([IPEnv isOs7])
    {
        CGFloat barHeight = navBar.frame.size.height;
        CGFloat insetHeight = barHeight;
        BOOL adjustForStatus = ([IPEnv isPhone] || alwaysFullScreen);
        CGFloat statusBarHeight = self.statusBarHeight;
        
        if (adjustForStatus)
        {
            CGRect navBarFrame = navBar.frame;
            navBarFrame.size.height += statusBarHeight;
            navBar.frame = navBarFrame;
            barHeight -= statusBarHeight;
        }
        
        if (mainScroll)
        {
            CGRect brFrame = mainContent.frame;
            brFrame.size.height += barHeight;
            brFrame.origin.y -= barHeight;
            mainContent.frame = brFrame;
            mainScroll.contentInset = UIEdgeInsetsMake(insetHeight, 0, 0, 0);
        }
        else if (adjustForStatus)
        {
            CGRect brFrame = mainContent.frame;
            brFrame.size.height -= statusBarHeight;
            brFrame.origin.y += statusBarHeight;
            mainContent.frame = brFrame;

        }
    }
}

-(BOOL)showingThumbs
{
    return NO;
}
-(UITableViewCell*)buildLoadingMoreCell:(UITableView *)tableView dark:(BOOL)dark
{
    static NSString* waitCellId = @"WaitCell";
    static int magicTag = 1234567;
    UITableViewCell * lastCell = [tableView dequeueReusableCellWithIdentifier:waitCellId];
    if (!lastCell)
    {
        lastCell = [[PhotoCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:waitCellId];
        
        IPWaitLabel *waitLabel = [[IPWaitLabel alloc] initWithStyle:(dark) ? FSWaitLabelStyleBlackBox : FSWaitLabelStyleWhiteBox];
        waitLabel.tag = magicTag;
        waitLabel.frame = CGRectInset(lastCell.bounds, 8, 6) ;
        waitLabel.text = IPLS(@"RetrievingMorePhotos");
        waitLabel.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight;
        waitLabel.isAnimating = YES;
        waitLabel.layer.cornerRadius = 4;
        [lastCell addSubview:waitLabel];
        lastCell.backgroundColor = [UIColor clearColor];
    }
    else
    {
        IPWaitLabel *waitLabel = SAFE_CAST([lastCell viewWithTag:magicTag],IPWaitLabel);
        waitLabel.isAnimating = YES;
    }
    return lastCell;
}

-(IPWaitLabel*)setupLoadingLabel:(BOOL)coverFullArea
{
    UIView *parentView = self.view;
    FSWaitLabelStyle style = (coverFullArea) ? FSWaitLabelStyleGray : FSWaitLabelStyleBlackBox;
	IPWaitLabel* label = [[IPWaitLabel alloc] initWithStyle:style];
	if (coverFullArea)
	{
		label.text = NSLocalizedString(@"Loading",@"");
		[label sizeToFit];
		label.frame = parentView.frame;
		label.tag = 0;
	}
	else
	{
		label.text = NSLocalizedString(@"Refreshing",@"");
		[label sizeToFit];
		CGFloat lWidth=160,lHeight=36;
		label.frame = CGRectMake((parentView.width-(lWidth*1.1)), (parentView.height-(lHeight*1.1)),1 ,1);
		label.autoresizingMask = UIViewAutoresizingFlexibleTopMargin|UIViewAutoresizingFlexibleLeftMargin;// | UIViewAutoresizingFlexibleHeight;
		label.frame = CGRectIntegral(CGRectMake((parentView.width-(lWidth*1.1)), (parentView.height-(lHeight*1.1)),lWidth ,lHeight));
		label.tag = 1;
		label.layer.cornerRadius = 8;
	}
	
	[parentView addSubview:label];
	return label;
}

-(void)viewWillAppear:(BOOL)animated
{
    [super viewWillAppear:animated];
#if !defined(FS_MAC)
    if ([IPEnv isIpad])
        self.navigationController.navigationBar.titleTextAttributes = @{UITextAttributeFont:[UIFont boldSystemFontOfSize:15]};
#endif
    if (secondTimeAppear && self.extraComplexHeader)
        [self.extraComplexHeader fullViewReappear];
    secondTimeAppear = YES;
}

-(void)viewWillDisappear:(BOOL)animated
{
    scrollDraggingForHideBar = NO;
    //hidingNavBar = NO;
    if ([IPEnv isIpad] && self.menuPopover)
        [self dismissPopoverAnimated:NO];

    // dismis sort if present
    if (sortOptions)
    {
        [sortOptions dismissWithClickedButtonIndex:0 animated:YES];
        sortOptions = nil;
    }
    [super viewWillDisappear:animated];
}

- (void)refreshPhotoSource:(BOOL)doRefresh
{
    
}

-(void)setupRefreshHeaderView
{
    if (self.supportsRefresh)
    {
        if (refreshHeaderView == nil)
        {
            UIScrollView *scrollV = self.scrollView;
            
            if (scrollV)
            {
                refreshHeaderView = [[DragRefreshView alloc] initWithScrollView:scrollV lightMode:YES];
            }
        }
        else
        {
            [refreshHeaderView handleRotate];
        }
    }
}

const CGFloat refreshDragLimit = -70.0f;


-(void)scrollViewWillBeginDragging:(UIScrollView *)scrollView
{
    lastScrollPos = self.tableView.contentOffset.y;
    scrollDraggingForHideBar = YES;
}

#if !defined(FS_MAC)
-(UIStatusBarAnimation)preferredStatusBarUpdateAnimation
{
    return UIStatusBarAnimationFade;
}
#endif

- (BOOL)prefersStatusBarHidden
{
    return self.navigationController.navigationBarHidden;
}

-(BOOL)shouldHideNavigationBarOnScroll
{
    return (hideNavBarPref && !self.editing && !self.extraPhotoHeader);
}
-(BOOL)isPopover
{
    return NO;
}
-(void)hideNavigationBar
{
    if (processingHideShowBar)
        return;
    // scrolling down, hide the navigation bar
    BOOL shouldHideNav = self.shouldHideNavigationBarOnScroll ;
    hidingNavBar = !self.extraPhotoHeader;
    processingHideShowBar = YES;
    
    [UIView animateWithDuration:0.1 animations:^
    {
        if (self.extraComplexHeader)
            [self.extraComplexHeader headerWillAppear:NO];
        if (shouldHideNav)
            [self hideMainNavigationBar:YES];
        if (hidingNavBar)
            self.iPhoneSecondBar.alpha = 0.0;
    }
    completion:^(BOOL finished)
    {
        if (hidingNavBar && self.iPhoneSecondBar)
        {
            UIEdgeInsets tableInset = self.tableView.contentInset;
            tableInset.top -= self.iPhoneSecondBar.bounds.size.height;
            self.tableView.contentInset = tableInset;
        }
        processingHideShowBar = NO;
     }];
}
-(void)resetStatusAndNavigationBars
{
    if ([IPEnv isOs7])
        [self setNeedsStatusBarAppearanceUpdate];
    [self hideNavigationBar];
    [self showNavigationBar];
}
-(void)showNavigationBar
{
    if (hidingNavBar)
    {
        if (!processingHideShowBar)
        {
            processingHideShowBar = YES;
            // scrolling down, hide the navigation bar

            BOOL shouldHideNav = self.shouldHideNavigationBarOnScroll ;
            // scrolling up, show the bars again
            hidingNavBar = NO;
            scrollDraggingForHideBar = NO;
            [UIView animateWithDuration:0.1 animations:^
            {
                if (self.extraComplexHeader)
                    [self.extraComplexHeader headerWillAppear:YES];

                if (shouldHideNav)
	                [self hideMainNavigationBar:NO];
                if (self.iPhoneSecondBar)
                {
                    self.iPhoneSecondBar.alpha = 1.0;
                    CGRect barFr = self.iPhoneSecondBar.frame;
                    barFr.origin.y = ([IPEnv isOs7]) ? self.navigationController.navigationBar.height + self.statusBarHeight : 0;
                    self.iPhoneSecondBar.frame = barFr;
                }
            }
            completion:^(BOOL finished)
            {
                if (self.iPhoneSecondBar)
                {
                    UIEdgeInsets tableInset = self.tableView.contentInset;
                    tableInset.top += self.iPhoneSecondBar.bounds.size.height;
                    self.tableView.contentInset = tableInset;
                }
                processingHideShowBar = NO;
            }];
        }
    }
    else
    {
        if ([IPEnv isOs7])
            [self setNeedsStatusBarAppearanceUpdate];
        
        [self hideMainNavigationBar:NO];
    }
}

-(void)hideMainNavigationBar:(BOOL)hidden
{
    self.navigationController.navigationBarHidden = ([IPEnv isMac]) ? YES : hidden;
}
- (void)scrollViewDidScroll:(UIScrollView *)scrollView
{
	if (scrollView.isDragging)
    {
        if ([FSPreferences instance].offlineMode)
        {
            refreshHeaderView.willRefresh = RefreshUnable;
            return;
        }
        CGFloat effectiveOffset = scrollView.contentOffset.y + scrollView.contentInset.top;
		if (refreshHeaderView && refreshHeaderView.willRefresh!=RefreshNo  && effectiveOffset > refreshDragLimit && effectiveOffset < 0.0f && !loadingLabel)
        {
			refreshHeaderView.willRefresh = RefreshNo;
		}
        
        else if (refreshHeaderView && refreshHeaderView.willRefresh!=RefreshYes && effectiveOffset < refreshDragLimit && !loadingLabel)
        {
			refreshHeaderView.willRefresh = RefreshYes;
        }
	}
    
    // This section is to autohide the navigation bar and/or the iphone secondary bar
    CGFloat newScrollPos = self.tableView.contentOffset.y;
    BOOL shouldHideNav = self.shouldHideNavigationBarOnScroll;
    if (scrollDraggingForHideBar && (shouldHideNav || self.iPhoneSecondBar)) // always hide the iphone bar automatically
    {
        // if we're bouncing at the bottom, ignore the scroll messages
        BOOL bouncingBottom  = ( scrollView.contentOffset.y > scrollView.contentSize.height - scrollView.frame.size.height + scrollView.contentInset.bottom );
        if (bouncingBottom)
            return;
        
        if (newScrollPos > lastScrollPos+10 && !hidingNavBar && scrollView.contentSize.height > scrollView.height*1.5)
            [self hideNavigationBar];
        else if (newScrollPos < lastScrollPos && hidingNavBar)
            [self showNavigationBar];
    }
    lastScrollPos = self.tableView.contentOffset.y;
}
- (void)scrollViewDidEndDragging:(UIScrollView *)scrollView willDecelerate:(BOOL)decelerate
{
	if (self.refreshHeaderView && scrollView.contentOffset.y <= refreshDragLimit && !self.loadingLabel && self.refreshHeaderView.willRefresh==RefreshYes)
    {
        scrollDraggingForHideBar = NO;
        self.userInitiatedRefresh = YES;
        [self refreshPhotoSource:YES];
	}
}

- (void)removeToolbarPressedLabel
{
    if (toolbarPressedLabel)
    {
        [toolbarPressedLabel quickDisappear];
        self.toolbarPressedLabel = nil;
    }
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Toolbar buttons
//

-(UIBarButtonItem*)createBarButtonItemWithImageName:(NSString*)imageName action:(SEL)action accessibleLabel:(NSString*)accessibleLabel width:(CGFloat)width buttonId:(FSToolbarButton)buttonId
{
    UIBarButtonItem* newButton;
    if ([IPEnv isOs7])
    {
        imageName = [@"topt-" stringByAppendingString:imageName];
        UIImage *image =  [UIImage imageNamed:imageName] ;// imageWithRenderingMode:UIImageRenderingModeAlwaysTemplate];

        newButton = [[UIBarButtonItem alloc] initWithImage:image style:UIBarButtonItemStylePlain target:self action:action];
        newButton.tag = buttonId;
    }
    else
    {
        IPToolbarButton *regularButton = [[IPToolbarButton alloc] initWithSize:CGSizeMake(width, 28) imageName:imageName];
        newButton = [[UIBarButtonItem alloc] initWithCustomView:regularButton];
        newButton.target = self;
        newButton.action = action;
        newButton.tag = buttonId;
        regularButton.parentItem = newButton;
        if (accessibleLabel)
            regularButton.accessibilityLabel = accessibleLabel;
    }
    if (accessibleLabel)
        newButton.accessibilityLabel = accessibleLabel;
    return newButton;
}

-(UIBarButtonItem*)addButtonToArray:(NSMutableArray*)buttons imageName:(NSString*)imageName action:(SEL)action accessibleLabel:(NSString*)accessibleLabel width:(CGFloat)width buttonId:(FSToolbarButton)buttonId
{
    UIBarButtonItem* newButton = [self createBarButtonItemWithImageName:imageName action:action accessibleLabel:accessibleLabel width:width buttonId:buttonId];
    [buttons addObject:newButton];
    return newButton;
}

-(UIBarButtonItem*)addButtonToArray:(NSMutableArray*)buttons systemButton:(UIBarButtonSystemItem)system action:(SEL)action accessibleLabel:(NSString*)accessibleLabel width:(CGFloat)width buttonId:(FSToolbarButton)buttonId
{
    if (![IPEnv isOs7] && system!=UIBarButtonSystemItemCancel)
    {
        NSString *image = (system==UIBarButtonSystemItemAction) ? @"actionMenu.png" : @"addMenu.png";
        return [self addButtonToArray:buttons imageName:image action:action accessibleLabel:accessibleLabel width:width buttonId:buttonId];
    }
    UIBarButtonItem* newButton = [[UIBarButtonItem alloc] initWithBarButtonSystemItem:system target:self action:action];
    if (accessibleLabel)
        newButton.accessibilityLabel = accessibleLabel;
    [buttons addObject:newButton];
    return newButton;
}

-(void)setupActionButtonStyle:(UIButton*)button
{
    if (![IPEnv isOs7] && button)
    {
        UIImage *newImage = [[UIImage imageNamed:@"BlueB.png"] stretchableImageWithLeftCapWidth:20 topCapHeight:0];
        [button setBackgroundImage:newImage forState:UIControlStateNormal];
        [button setTitleColor:[UIColor whiteColor] forState:UIControlStateNormal];
        button.titleLabel.font = [UIFont boldSystemFontOfSize:14];
    }
}

-(BOOL)createToolbarWithOptionalButtons:(FSToolbarButton)optionalButtons refresh:(BOOL)refresh
{
	if ((!refresh &&  self.navigationItem.rightBarButtonItems))  // toolbar already created
    {
        return NO;
	}
    currentToolbarButtons = optionalButtons;
    
	// create the array to hold the buttons, which then gets added to the toolbar
	NSMutableArray* buttons = [[NSMutableArray alloc] initWithCapacity:6];
	NSMutableArray* buttonsDown = buttons;
    
    NSInteger estimatedCount = 0;
    for (NSInteger i=0; i < 32; i++)
    {
        if ((optionalButtons & (1 << i)) != 0)
            estimatedCount++;
    }
    
    BOOL hideHome = NO;
    if (optionalButtons & FSToolbarButtonVote)
    {
        if ([IPEnv isPhone])
            hideHome = YES;
        estimatedCount=6;
    }
    
    BOOL useLowerBar = NO;
    if ( estimatedCount>= 6 && [IPEnv isPhone])
    {
        buttonsDown = [[NSMutableArray alloc] initWithCapacity:3];
        [buttonsDown addObject:[[UIBarButtonItem alloc] initWithBarButtonSystemItem:UIBarButtonSystemItemFlexibleSpace target:nil action:nil]];
        useLowerBar = YES;
    }
    
	BOOL hasEdit = (self.viewSituation != VSLEFT) && ( ((optionalButtons & FSToolbarButtonEdit) && ![FSPreferences instance].offlineMode) || (optionalButtons & FSToolbarButtonEditForce));
    CGFloat iPhoneBarWidth = [IPEnv isOs7] ? 38 : 35;
    
    // width is onlhy really needed for ios6
    CGFloat buttonWidth = ([IPEnv isPhone] ? 42 : 46);
    
	if ([self conformsToProtocol:@protocol(FSViewController)])
	{
		if (self.viewSituation != VSLEFT)
		{
            if ((optionalButtons & FSToolbarButtonCancel))
                [self addButtonToArray:buttons systemButton:UIBarButtonSystemItemCancel action:@selector(cancel:) accessibleLabel:nil width:buttonWidth buttonId:FSToolbarButtonCancel];
            
            if ((!(optionalButtons & FSToolbarButtonNoAction) || (optionalButtons & FSToolbarButtonHome)) && !hideHome)
			{
                [self addButtonToArray:buttons imageName:@"top-home.png" action:@selector(homePressed:) accessibleLabel:IPLS(@"AccButHome") width:buttonWidth buttonId:FSToolbarButtonHome];
			}

            if (hasEdit)
			{
                if (self.editButtonItem)
                {
                    [self addButtonToArray:buttons imageName:@"select.png" action:@selector(editPressed:) accessibleLabel:IPLS(@"Select") width:buttonWidth buttonId:FSToolbarButtonEdit];
                }
			}

            if (optionalButtons & FSToolbarButtonSort)
            {
                UIBarButtonItem *newB = [self addButtonToArray:buttonsDown imageName:@"sort.png" action:@selector(sortPressed:) accessibleLabel:IPLS(@"Sort") width:buttonWidth buttonId:FSToolbarButtonSort];
                if (useLowerBar)
                    newB.width = iPhoneBarWidth;
            }
            
            if (optionalButtons & FSToolbarButtonAdd)
            {
                UIBarButtonItem *newB = [self addButtonToArray:buttonsDown systemButton:UIBarButtonSystemItemAdd action:@selector(addItem:) accessibleLabel:IPLS(@"AccButAddPhotos") width:buttonWidth buttonId:FSToolbarButtonAdd];
                if (useLowerBar)
                    newB.width = iPhoneBarWidth;
            }
            
            if ([FSPreferences instance].offlineMode)
                [self addButtonToArray:buttons imageName:@"offline.png" action:@selector(offlinePressed:) accessibleLabel:IPLS(@"OfflineMode") width:buttonWidth buttonId:0];

            
            if (optionalButtons & FSToolbarButtonJoinGroup)
            {
                UIBarButtonItem *newB = [self addButtonToArray:buttonsDown imageName:@"joingroup.png" action:@selector(joinGroupPressed:) accessibleLabel:IPLS(@"AccButGroupDiscussions") width:buttonWidth buttonId:FSToolbarButtonJoinGroup];
                if (useLowerBar)
                    newB.width = iPhoneBarWidth;
            }

            if (optionalButtons & FSToolbarButtonLeaveGroup)
            {
                UIBarButtonItem *newB = [self addButtonToArray:buttonsDown imageName:@"leavegroup.png" action:@selector(joinGroupPressed:) accessibleLabel:IPLS(@"AccButGroupDiscussions") width:buttonWidth buttonId:FSToolbarButtonLeaveGroup];
                if (useLowerBar)
                    newB.width = iPhoneBarWidth;
            }
            
			if (optionalButtons & FSToolbarButtonGroupDiscussions)
            {
                UIBarButtonItem *newB = [self addButtonToArray:buttonsDown imageName:@"top-discuss.png" action:@selector(discussionsPressed:) accessibleLabel:IPLS(@"AccButGroupDiscussions") width:buttonWidth buttonId:FSToolbarButtonGroupDiscussions];
                if (useLowerBar)
                    newB.width = iPhoneBarWidth;
            }
            
            
			if (optionalButtons & FSToolbarButtonExploreDate)
            {
                UIBarButtonItem *newB =[self addButtonToArray:buttonsDown imageName:@"calendar.png" action:@selector(explorePressed:) accessibleLabel:IPLS(@"AccButExploreDate") width:buttonWidth buttonId:FSToolbarButtonExploreDate];
                if (useLowerBar)
                    newB.width = iPhoneBarWidth;
            }
            
            if (optionalButtons & FSToolbarButtonCategory)
            {
                UIBarButtonItem *newB = [self addButtonToArray:buttonsDown imageName:@"category-mini.png" action:@selector(categoryPressed:) accessibleLabel:IPLS(@"AccButExploreDate") width:buttonWidth buttonId:FSToolbarButtonCategory];
                if (useLowerBar)
                    newB.width = iPhoneBarWidth;
            }
            
			if (((optionalButtons & FSToolbarButtonFavorite) || (optionalButtons & FSToolbarButtonFavoriteHeart) || (optionalButtons & FSToolbarButtonFavoriteThumb) || ((optionalButtons & FSToolbarButtonSocialNet) && !([AppSpecific isCustom]))) && ![FSPreferences instance].offlineMode)
			{
				NSString *favImage;

                NSInteger tagBase = ((optionalButtons & FSToolbarButtonFavoriteHeart)) ? 1 : ((optionalButtons & FSToolbarButtonFavoriteThumb)) ? 2  : 0;
                NSString *baseTitle = (tagBase==1) ? @"500-" : (tagBase==2) ? @"fb-" : @"";
                
                if (optionalButtons & FSToolbarButtonSocialNet)
                    favImage = @"top-socnet";
                else
                    favImage = [NSString stringWithFormat:@"%@mini-fav.png",baseTitle] ;
                
				NSString *favAccess = (optionalButtons & FSToolbarButtonSocialNet) ? @"AccButSocNet" : @"AccButAddToFavorites";

                FSToolbarButton buttonId = optionalButtons & (FSToolbarButtonFavorite|FSToolbarButtonFavoriteHeart|FSToolbarButtonFavoriteThumb|FSToolbarButtonSocialNet);
                [self addButtonToArray:buttons imageName:favImage action:@selector(favoritePressed:) accessibleLabel:IPLS(favAccess) width:buttonWidth buttonId:buttonId];
			}
            
			if ((optionalButtons & FSToolbarButtonVote))
                [self addButtonToArray:buttons imageName:@"vote.png" action:@selector(votePressed:) accessibleLabel:IPLS(@"AccButVote") width:buttonWidth buttonId:FSToolbarButtonVote];
			
			if ((optionalButtons & FSToolbarButtonStackOn) || (optionalButtons & FSToolbarButtonStack))
			{
				NSString *stackImage = (optionalButtons & FSToolbarButtonStackOn) ? @"mini-stack-on.png" : @"mini-stack-off.png";
				NSString *stackAccess = (optionalButtons & FSToolbarButtonSocialNet) ? @"AccButRemoveFromStack" : @"AccButAddToStack";
                FSToolbarButton buttonId = optionalButtons & (FSToolbarButtonStackOn|FSToolbarButtonStack);
                [self addButtonToArray:buttons imageName:stackImage action:@selector(stackPressed:) accessibleLabel:IPLS(stackAccess) width:buttonWidth buttonId:buttonId];
			}

			
            if ((optionalButtons & FSToolbarButtonHelp))
                [self addButtonToArray:buttons imageName:@"newhelp.png" action:@selector(about:) accessibleLabel:IPLS(@"AccButHelp") width:buttonWidth buttonId:FSToolbarButtonHelp];

            if ((optionalButtons & FSToolbarButtonDebug))
                [self addButtonToArray:buttons systemButton:UIBarButtonSystemItemCompose action:@selector(sendDebugInfo:) accessibleLabel:nil width:buttonWidth buttonId:FSToolbarButtonDebug];

			if (!(optionalButtons & FSToolbarButtonNoAction) || (optionalButtons & FSToolbarButtonAction))
                [self addButtonToArray:buttons systemButton:UIBarButtonSystemItemAction action:@selector(doActionPressed:) accessibleLabel:nil width:buttonWidth buttonId:FSToolbarButtonAction];
            
		}
	}
	
	if (optionalButtons & FSToolbarButtonAllSwitch)
	{
		static NSString * iconNames[5] = {@"", @"details.png",  @"thumbs.png", @"onlyphoto.png", @"detailsonphoto.png"};
		static NSString * accessNames[5] = {@"", @"AccButList",  @"AccButThumbsg", @"AccButPhoto", @"AccButDetailsPhoto"};
		int index = (optionalButtons & FSToolbarButtonAllSwitch);
        [self addButtonToArray:buttons imageName:iconNames[index] action:@selector(switchToAlternate:) accessibleLabel:IPLS(accessNames[index]) width:buttonWidth buttonId:index];
	}
	
	NSUInteger nButtons = [buttons count];
    if ([FSPreferences instance].offlineMode && nButtons==3)
    {
        [buttons removeObjectAtIndex:1];
    }
    
    
    NSMutableArray *newArray = [[NSMutableArray alloc] initWithCapacity:buttons.count];
    for (id eachButton in buttons.reverseObjectEnumerator)
        [newArray addObject:eachButton];
    self.navigationController.navigationBar.tintColor = [FSThemeManager toolbarButtonColor];
    if ([IPEnv isMac])
    {
        [self.windowController setToolbarButtons:newArray];
    }
    else
    {
        self.navigationItem.rightBarButtonItems = newArray;
    }
    
    if (buttonsDown != buttons)
    {
        iPhoneSecondBarButtons = buttonsDown;
    }
    return YES;
}

-(void)editPressed:(id)sender
{
    SEL regularEditAction = self.editButtonItem.action;
    
    NSMethodSignature *signature = [[self class] instanceMethodSignatureForSelector:regularEditAction];
    NSInvocation *invocation = [NSInvocation invocationWithMethodSignature:signature];
    invocation.selector = regularEditAction;
    [invocation setArgument:&sender atIndex:2];          // note that the first real argument has index 2!
    [invocation invokeWithTarget:self];
}
- (void)setEditing:(BOOL)editing animated:(BOOL)animate
{
    NSString *imageName = !editing ? @"select" : @"select-invert";
    [self switchToolbarIconWithActionSelector:@selector(editPressed:) imageName:imageName accessibilityLabel:IPLS(@"Select")];
    [super setEditing:editing animated:animate];
}

-(UIBarButtonItem*)switchToolbarIconWithActionSelector:(SEL)selector imageName:(NSString*)newName accessibilityLabel:(NSString*)accessibilty
{
    NSMutableArray* buttons = [[NSMutableArray alloc] initWithArray:self.navigationItem.rightBarButtonItems];
    NSInteger indexToReplace=-1;
    UIImage *newImage = nil;
    NSString *imageName;
    for (NSUInteger nBut = 0; nBut<buttons.count;nBut++)
    {
        UIBarButtonItem* eachButton = buttons[nBut];
        if (eachButton.action == selector)
        {
            imageName = newName;
            
            NSInteger tagBase = ((eachButton.tag & FSToolbarButtonFavoriteHeart)) ? 1 : ((eachButton.tag & FSToolbarButtonFavoriteThumb)) ? 2  : 0;
            if (tagBase> 0)
            {
                NSString *baseTitle = (tagBase==1) ? @"500-" : @"fb-";  // handle fav icons
                imageName = [baseTitle stringByAppendingString:newName];
            }
            
            newImage = [UIImage imageNamed:imageName];
            if (eachButton.image == newImage)
            {
                return eachButton;		// same image, don't bother redrawing
            }
            indexToReplace = nBut;
            break;
        }
    }
    
    if (indexToReplace>=0)
    {
        UIBarButtonItem* foundToReplace = buttons[indexToReplace];
        CGFloat buttonWidth = (foundToReplace.customView) ? foundToReplace.customView.bounds.size.width : foundToReplace.width;
        UIBarButtonItem* replace = [self createBarButtonItemWithImageName:imageName action:foundToReplace.action accessibleLabel:accessibilty width:buttonWidth buttonId:foundToReplace.tag];
        buttons[(NSUInteger)indexToReplace] = replace;
        self.navigationItem.rightBarButtonItems = buttons;
        return replace;
    }
    return nil;
}

-(void)replaceToolbarIconWithState:(ToolbarButtonSetState)state imageView:(UIImageView*)imageViewToUpdate
{
	static NSString* imagesForFav[] = {@"mini-fav.png",@"mini-fav-on.png",@"mini-fav-off.png",@"top-socnet.png", @"mini-stack-on.png", @"mini-stack-off.png", @"fb-mini-fav-off.png", @"fb-mini-fav-on.png"};
	static NSString* accessForFav[] = {@"AccButAddToFavorites",@"AccButAddToFavorites",@"AccButRemoveFromFavorites",@"AccButSocNet", @"AccButRemoveFromStack", @"AccButAddToStack", @"AccButVote", @"AccButVoteNo"};
	
	SEL buttonSelector = (state >= TBSVoteYes) ?  @selector(votePressed:) :  ((state>=TBSStackYes) ? @selector(stackPressed:) : @selector(favoritePressed:));
    NSString* accLabel = IPLS(accessForFav[state]);
	UIBarButtonItem *newButton = [self switchToolbarIconWithActionSelector:buttonSelector imageName:imagesForFav[state] accessibilityLabel:accLabel];
    if (imageViewToUpdate)
    {
        if (newButton.customView)
        {
            IPToolbarButton *ipb = (IPToolbarButton*)newButton.customView;
            if ([IPEnv isOs7])
            {
                imageViewToUpdate.image =  [ [ipb imageForState:UIControlStateNormal] imageWithRenderingMode:UIImageRenderingModeAlwaysTemplate];
                imageViewToUpdate.tintColor = [FSThemeManager traslucentQuickBarTextColor];
            }
            else
                imageViewToUpdate.image =  [ipb imageForState:UIControlStateNormal];
        }
        else
        {
            if ([IPEnv isOs7])
            {
                imageViewToUpdate.image = [ newButton.image imageWithRenderingMode:UIImageRenderingModeAlwaysTemplate];
                imageViewToUpdate.tintColor = [FSThemeManager traslucentQuickBarTextColor];
            }
            else
            {
                imageViewToUpdate.image = newButton.image;
            }
        }
        
        imageViewToUpdate.accessibilityLabel = accLabel;
    }
}


-(void)pushNewSource:(ProviderPhotoSource*)newPhotos indexPath:(NSIndexPath*)indexPath
{
    FSBaseViewController *anotherViewController = [self createPreferredPhotoListView:newPhotos];
    [self pushNewSource:newPhotos indexPath:indexPath viewController:anotherViewController];
}

-(void)pushNewSource:(ProviderPhotoSource*)newPhotos indexPath:(NSIndexPath*)indexPath viewController:(FSBaseViewController*)anotherViewController
{
	FSBaseViewController *newLeft=nil;
	if (self.viewSituation==VSRIGHT)
	{
		// Navigation logic may go here -- for example, create and push another view controller.
		newLeft = [self cloneWithViewSituation:VSLEFT];
	}
	if (self.viewSituation==VSLEFT)
	{
		[self.windowController.state removeLastStep];
		[self pushViewControllerOnRight:anotherViewController atTop:NO dismissPopover:YES];
	}
	else
	{
		[self pushViewControllerFromRightToLeft:newLeft newRight:anotherViewController];
	}
	// Save state
	[self.windowController.state addStep:anotherViewController photoSource:newPhotos indexForPreviousStep:indexPath.row sectionForPreviousStep:indexPath.section];
}

-(void)showSendPopover:(CommonPopoverController*)content button:(id)sender action:(CommonPopoverBlock)actionBlock
{
    if (self.menuPopover)
        return;
    FSPopoverController* aPopover = [content displayFromSender:sender action:actionBlock];
	self.menuPopover = aPopover;  		// Store the popover in a custom property for later use.
}

- (void)popoverDismissed:(UIViewController*)contentController
{
	[self dismissPopover];
}

-(void)dismissPopover
{
    [self dismissPopoverAnimated:YES];
}
- (void)dismissPopoverAnimated:(BOOL)animated;
{
    @try
    {
        if (self.menuPopover)
        {
            [self.menuPopover dismissPopoverAnimated:animated];
            self.menuPopover=nil;
        }
        if (![IPEnv isIpad])
        {
            [self dismissViewControllerAnimated:animated completion:nil];
        }
    }
    @catch (NSException *exception)
    {
        NSLog(@"EX(dismissPopover) %@",exception);
    }
}
- (void)popoverControllerDidDismissPopover:(UIPopoverController *)popoverController
{
    self.menuPopover = nil;
}
-(void)replaceToolbarIcon:(ToolbarButtonSetState)state
{
	[self replaceToolbarIconWithState:state imageView:nil];
}


-(id<PhotoSetExtendedView>)photoSetView
{
    if ([self conformsToProtocol:@protocol(PhotoSetExtendedView)])
        return (id<PhotoSetExtendedView>)self;
    return nil;
}

-(FSBaseViewController*)cloneWithViewSituation:(ViewSituation)newViewSituation
{
    return nil;
}

-(ProviderPhotoSource*)createFilteredSource:(ProviderPhoto*)existingPhoto newPhoto:(ProviderPhoto**)newPhoto
{
    ProviderPhoto *resultPhoto = existingPhoto;
    ProviderPhotoSource *source = existingPhoto.photoSource;
    ProviderPhotoSource *filteredSource = source.filteredPhotoSource;
    if (filteredSource != source)
    {
        ProviderPhoto *newFoundPhoto = filteredSource[resultPhoto.pid];
        if (newFoundPhoto)
        {
            resultPhoto = newFoundPhoto;
        }
        else if (filteredSource.numberOfPhotos>0)
        {
            resultPhoto = filteredSource[0];
        }
    }
    
    if (newPhoto)
        *newPhoto = resultPhoto;
    return filteredSource;
}

-(void)tipLabelDisappeared:(TipLabel*)label
{
    [self removeToolbarPressedLabel];
}

-(void)setupIPhoneSecondBarForceRecreate:(BOOL)forceRecreate
{
    BOOL showTitle = self.showTitleInTableHeader && !self.extraComplexHeader;
    BOOL cleaned = NO;
    static CGFloat minTitleHeight = 22;
    CGFloat photoExtraHeight = (![IPEnv isOs7] && [IPEnv isIpad]) ? 85 : 83;

    UIEdgeInsets currentTableInset = self.tableView.contentInset;
    CGPoint currentTableOffset = self.tableView.contentOffset;
    if (self.iPhoneSecondBar && forceRecreate)
    {
        CGFloat oldHeight = self.iPhoneSecondBar.bounds.size.height;
        currentTableInset.top -= oldHeight;
        currentTableOffset.y += oldHeight;
        [self.iPhoneSecondBar removeFromSuperview];
        self.iPhoneSecondBar = nil;
        cleaned = YES;
    }
    
    if (!self.iPhoneSecondBar && (self.showTitleInTableHeader || self.extraPhotoHeader || self.extraComplexHeader))
    {
        CGFloat standardHeight = ((iPhoneSecondBarButtons) ? ([IPEnv isOs7] ? 44 : 38) : minTitleHeight);
        CGFloat titleHeight = (self.showTitleInTableHeader) ? minTitleHeight : 0;

        CGFloat extraComplexHeight = self.extraComplexHeader.height;

        CGFloat height =  (self.extraComplexHeader) ? extraComplexHeight : ((self.extraPhotoHeader) ? titleHeight+photoExtraHeight : standardHeight);
        
        self.iPhoneSecondBar = [self createBlackSectionHeader:(showTitle ? self.title : nil) textHeight:(self.extraPhotoHeader || self.extraComplexHeader) ? minTitleHeight : standardHeight];
        IPBlurView *blur = SAFE_CAST(self.iPhoneSecondBar, IPBlurView);
        
        if (self.extraComplexHeader)
        {
            CGRect barBounds = self.iPhoneSecondBar.bounds;
            UIView *complexView = self.extraComplexHeader.mainView;
            complexView.frame = CGRectMake(0, 0, barBounds.size.width, extraComplexHeight);
            complexView.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight;
            [self.iPhoneSecondBar addSubview:complexView];
            if (iPhoneSecondBarButtons)
            {
                // mini second bar
                CGFloat width = 44 * (iPhoneSecondBarButtons.count-1);
                UIToolbar *subToolbar = [[UIToolbar alloc] initWithFrame:CGRectMake(blur.frame.size.width - width, 0, width, 44)];
                subToolbar.tintColor = [FSThemeManager toolbarButtonColor];
                subToolbar.autoresizingMask = UIViewAutoresizingFlexibleLeftMargin | UIViewAutoresizingFlexibleBottomMargin;
                subToolbar.items = iPhoneSecondBarButtons;
                [IPBlurView removeBackgroundFromToolbar:subToolbar];
                [blur addSubview:subToolbar];
            }
        }
        else if (blur && iPhoneSecondBarButtons && !self.extraPhotoHeader)
        {
            [blur setToolbarButtons:iPhoneSecondBarButtons];
            if (!self.extraComplexHeader)
            {
                UILabel *lab = SAFE_CAST([self.iPhoneSecondBar viewWithTag:100], UILabel);
                lab.width = lab.width - (iPhoneSecondBarButtons.count * 34);
            }
        }
        

        else if (self.extraPhotoHeader)
        {
            PhotoSize size = ([self.extraPhotoHeader.photo getImagePath:PLSMALL].length) ? PLSMALL : PLTHUMB;
            CGFloat photoWidth = (size==PLTHUMB) ? 48 : 100;
            UIImageView *photoView = [[UIImageView alloc] initWithFrame:CGRectMake(4, (titleHeight==0) ? 4 : titleHeight, photoWidth, photoExtraHeight-8)];
            photoView.contentMode = UIViewContentModeScaleAspectFit;
            ImageRequest *photoReq = [[ImageRequest alloc] initWithPhoto:self.extraPhotoHeader.photo size:size];
            [photoReq sendWithImageResponse:^(UIImage *image, NSError *error)
            {
                 photoView.image = image;
            }];
            [self.iPhoneSecondBar addSubview:photoView];
            
            CGFloat messageWidth = self.iPhoneSecondBar.bounds.size.width - 130;
            UILabel *message = [[UILabel alloc] initWithFrame:CGRectMake(110, titleHeight, messageWidth, 50)];
            message.font = [UIFont systemFontOfSize:(titleHeight) ? 12 : 16];
            message.numberOfLines = 3;
            message.text = self.extraPhotoHeader.message;
            message.textColor = [FSThemeManager traslucentMessageTextColor];
            message.backgroundColor = [UIColor clearColor];
            message.autoresizingMask = UIViewAutoresizingFlexibleWidth;
            [self.iPhoneSecondBar addSubview:message];
            NSArray *buttons = self.extraPhotoHeader.buttons;
            CGFloat xOrigin = 110;
            CGFloat buttonSpacing = [IPEnv isIpad] ? 90 : 65;
            for (UIButton* eachButton in buttons)
            {
                eachButton.frame = CGRectMake(xOrigin, (titleHeight) ? (titleHeight + 44) : 30, buttonSpacing - 10, (titleHeight) ? 30 : 44);
                eachButton.titleLabel.font = [UIFont boldSystemFontOfSize:([IPEnv isIpad]) ? 20 : 18];

                [self.iPhoneSecondBar addSubview:eachButton];
                xOrigin += buttonSpacing;
            }
        }

        CGFloat currentTopInset = currentTableInset.top;
        if (currentTopInset==0 && [IPEnv isOs7] && ([IPEnv isPhone] || !self.isPopover))
        {
            currentTopInset = self.navigationController.navigationBar.bounds.size.height + self.statusBarHeight;
        }
        CGRect frame = CGRectMake(0, currentTopInset, self.tableView.bounds.size.width, height);
        self.iPhoneSecondBar.frame = frame;
        [self.view addSubview:self.iPhoneSecondBar];
        UIEdgeInsets tableInset = currentTableInset;
        CGPoint tableOffset = currentTableOffset;
        tableInset.top += height;
        tableOffset.y -= height;
        self.tableView.contentOffset = tableOffset;
        self.tableView.contentInset = tableInset;
    }
    else if (cleaned)
    {
        self.tableView.contentOffset = currentTableOffset;
        self.tableView.contentInset = currentTableInset;
    }
}

-(void)addEmptyLabel:(NSString*)message
{
	UIView *viewToAdd = (self.tableView) ? self.tableView : self.view;
    UIEdgeInsets insets = (self.tableView) ? self.tableView.contentInset : UIEdgeInsetsZero;
    
	CGFloat margin = 50;
    UIFont *textFont =  [UIFont systemFontOfSize:20];
    CGSize textSize = [message sizeWithFont:textFont constrainedToSize:CGSizeMake(viewToAdd.width*2/3, viewToAdd.height*2/3) lineBreakMode:NSLineBreakByWordWrapping];
    textSize.width += margin;
    textSize.height += margin;
    
    CGRect textArea = CGRectIntegral( CGRectMake((viewToAdd.width-textSize.width)/2, (viewToAdd.height-(insets.top+insets.bottom)-textSize.height)/2 , textSize.width, textSize.height) );
    UIView *outerView = [[UIView alloc] initWithFrame:textArea];
    
    UILabel* label = [[UILabel alloc] initWithFrame:outerView.bounds];
    [outerView addSubview:label];
    label.text = message;
    label.font = textFont;
    label.textAlignment = NSTextAlignmentCenter;
    label.numberOfLines = 0;
    label.autoresizingMask = UIViewAutoresizingFlexibleWidth|UIViewAutoresizingFlexibleHeight;
    outerView.autoresizingMask = UIViewAutoresizingFlexibleWidth|UIViewAutoresizingFlexibleTopMargin|UIViewAutoresizingFlexibleBottomMargin;
    outerView.alpha = 0.0;
    if (self.parentViewController)
    {
        label.backgroundColor = [UIColor clearColor];
        outerView.backgroundColor = [FSThemeManager traslucentMessageBackgroundColor];
        label.textColor = [FSThemeManager traslucentMessageTextColor];
        outerView.layer.cornerRadius = 6;
    }
    else
    {
        outerView.backgroundColor = [UIColor clearColor];
        label.backgroundColor = [UIColor clearColor];
    }
    [viewToAdd addSubview:outerView];
    self.emptyLabel = outerView;
    
    [UIView animateWithDuration:0.8 delay:0 options:UIViewAnimationOptionCurveEaseIn animations:^
    {
         outerView.alpha = 1.0;
    } completion:nil];
}

-(void)extraHeaderUpdatedAnimated:(BOOL)animated
{
    if (animated)
    {
        self.refreshHeaderView.hidden = YES;
        [UIView animateWithDuration:0.25 animations:^
        {
            [self setupIPhoneSecondBarForceRecreate:YES];
        }
        completion:^(BOOL finished)
        {
            self.refreshHeaderView.hidden = NO;
        }];
    }
    else
    {
        [self setupIPhoneSecondBarForceRecreate:YES];
    }
}

-(void)rotateIPhoneSecondBar
{
    if (self.iPhoneSecondBar && !hidingNavBar)
    {
        CGRect frame = self.iPhoneSecondBar.frame;
        self.iPhoneSecondBar.top = self.tableView.contentInset.top - frame.size.height;
    }
    if (self.extraComplexHeader)
        [self.extraComplexHeader rotated];
}

-(void)adjustTabBar:(UITabBar*)tabBar
{
    if ([IPEnv isIpad] && [IPEnv osVersion] >= OsVersion71)
    {
        CGRect tabFrame = tabBar.frame;
        tabFrame.origin.y = self.view.bounds.size.height - (tabFrame.size.height+4);
        tabBar.frame = tabFrame;
    }
}

- (void)dismissViewControllerAnimated:(BOOL)animated completion:(void (^)(void))completion
{
    if ([IPEnv isMac])
    {
        [self macWindowClose];
        if (completion)
            completion();
    }
    else
    {
        [super dismissViewControllerAnimated:animated completion:completion];
    }
}
-(void)sortPressed:(id)sender
{
    if (sortOptions || [sender isKindOfClass:[ProviderPhotoSource class]])
        return;
    
    NSMutableArray *buttons;
    NSArray *sortTypes;
    NSInteger defaultIndex = [self.photos sortNames:&buttons sortTypes:&sortTypes];
    PhotoSourceSort serverSort = self.photos.sortsSupportedService;
    PhotoSourceSort sortCurrent = self.photos.currentSort;

    if ([IPEnv isIpad])
        [buttons addObject:@""];
    
    sortOptions = [[IPActionSheet alloc] initWithTitle:IPLS(@"Sort") cancelButtonIndex:defaultIndex otherButtonTitles:buttons];
    [sortOptions showInView:self.view sender:sender  selected:^(NSInteger buttonIndex)
    {
        PhotoSourceSort newSort = [sortTypes[buttonIndex] integerValue];
        if (newSort != sortCurrent)
        {
            BOOL needRefresh = ((serverSort & newSort) || (serverSort & sortCurrent)) ? YES : NO;
            self.photos.currentSort = newSort;
            NSArray *relatedCont = [self.windowController relatedControllers:self];
            for (FSBaseViewController *eachRelated in relatedCont)
            {
                [eachRelated sortPressed:self.photos];
            }
            
            if (needRefresh)
                [self refreshPhotoSource:YES];
            else
            {
                [self.photos refreshLocalSort];
            }
        }
        sortOptions = nil;
    }];
}

-(void)showOnMacWindow
{
    macWindowController = [[GenericUIKitWindowController alloc] initWithViewController:self];
    [macWindowController showWindow:self];
}

-(void)macWindowClose
{
    [macWindowController closeWindow];
}

-(void)macWindowDidClose:(BOOL)closePressed
{
    macWindowController = nil;
}
-(void)dealloc
{
    if (loadingLabel)
		[loadingLabel removeFromSuperview];

}

@end



