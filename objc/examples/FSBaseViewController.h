//
//  FSBaseViewController.h
//  FlickStackr
//
//  Created by Carlos Mejia on 2013-05-01.
//  Copyright (c) 2013 iPont. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "TipLabel.h"
#import "FSWindowController.h"

@class PhotoListEditHelper;
@class DragRefreshView;
@class TipLabel;
@class FSPopoverController;
@class PhotoCell;

extern const CGFloat refreshDragLimit;
typedef void (^QuickActionChooserCompleteBlock)(QuickAction selected);

@interface FSBaseViewController : UIViewController<FSViewController, TipLabelDelegate, FSPopoverNotification>
{
    @protected
    NSArray *quickActionRecognizers;
    PhotoListEditHelper *editHelper;
}
-(instancetype)initWithFSWindowController:(FSWindowController*)fsWindowController ;
-(instancetype)initWithNibName:(NSString *)nibNameOrNil windowController:(FSWindowController*)fsWindowController ;
-(UITableViewCell*)buildLoadingMoreCell:(UITableView *)tableView dark:(BOOL)dark;
-(void)pushViewControllerOnRightWithNewController:(UIViewController*)newController atTop:(NSInteger)atTop  dismissPopover:(BOOL)shouldDismissPopover animated:(BOOL)animated;
-(void)pushViewControllerFromRightToLeftWithCloned:(FSBaseViewController*)clonedController newRight:(UIViewController*)completelyNewController  dismissPopover:(BOOL)shouldDismissPopover animated:(BOOL)animated;
-(void)pushViewControllerOnRight:(UIViewController*)newController atTop:(NSInteger)atTop  dismissPopover:(BOOL)shouldDismissPopover;
-(void)pushViewControllerFromRightToLeft:(FSBaseViewController*)clonedController newRight:(UIViewController*)completelyNewController dismissPopover:(BOOL)shouldDismissPopover;
-(void)pushViewControllerFromRightToLeft:(FSBaseViewController *)completelyNewController;
-(void)pushViewControllerFromRightToLeft:(FSBaseViewController*)clonedController newRight:(UIViewController*)completelyNewController;
-(void)removeControllerFromStack:(FSBaseViewController*)toRemove;
-(FSBaseViewController*)movePhotoDetailViewToPhoto:(ProviderPhoto*)fp forcePhotoView:(BOOL)forcePhotoView  showOwner:(BOOL)showOwner;
-(FSBaseViewController*)movePhotoDetailViewToPhoto:(ProviderPhoto*)fp forcePhotoView:(BOOL)forcePhotoView;
-(FSBaseViewController*)createPreferredPhotoListViewForCurrentView:(id<FSViewController>)currentView photos:(ProviderPhotoSource*)inPhotos;
-(FSBaseViewController*)createPreferredPhotoListView:(ProviderPhotoSource*)inPhotos;
-(FSBaseViewController*)createPreferredPhotoViewForPhoto:(ProviderPhoto*)inPhoto viewSituation:(ViewSituation)viewSituation  forcePhotoView:(BOOL)forcePhotoView;
-(FSBaseViewController*)createPhotoViewFromClassName:(NSString*)className photo:(ProviderPhoto*)inPhoto viewSituation:(ViewSituation)viewSituation;
-(FSBaseViewController*)cloneWithViewSituation:(ViewSituation)newViewSituation;
-(void)movePhotoListViews:(ProviderPhoto*)photo;
-(void)showQuickActionChooser:(UIGestureRecognizer *)recognizer doubleTap:(BOOL)doubleTap selected:(QuickActionChooserCompleteBlock)selected;
-(NSArray*)setupQuickActionRecognizers;
-(NSArray*)setupQuickActionRecognizersWithOtherView:(UIView*)otherView;
-(UIView*)createBlackSectionHeader;
-(UIView*)createBlackSectionHeader:(NSString*)headerText textHeight:(CGFloat)textHeight;
-(IPWaitLabel*)setupLoadingLabel:(BOOL)coverFullArea;
-(BOOL)createToolbarWithOptionalButtons:(FSToolbarButton)buttons refresh:(BOOL)recreate;
-(UIBarButtonItem*)switchToolbarIconWithActionSelector:(SEL)selector imageName:(NSString*)newName  accessibilityLabel:(NSString*)accessibilty;
-(UIBarButtonItem*)createBarButtonItemWithImageName:(NSString*)imageName action:(SEL)action accessibleLabel:(NSString*)accessibleLabel width:(CGFloat)width buttonId:(FSToolbarButton)buttonId;
-(void)replaceToolbarIconWithState:(ToolbarButtonSetState)state imageView:(UIImageView*)imageViewToUpdate;
-(void)replaceToolbarIcon:(ToolbarButtonSetState)state;
-(void)setupRefreshHeaderView;
-(void)refreshPhotoSource:(BOOL)doRefresh;
-(void)removeToolbarPressedLabel;
-(void)setIphoneBackButton;
-(void)setIphoneTopTitle:(FSBaseViewController*)newController;
-(void)adjustNavigationBarForModal:(UINavigationBar*)navBar mainContent:(UIView*)mainContent mainScroll:(UIScrollView*)mainScroll alwaysFullScreen:(BOOL)alwaysFullScreen;
-(void)setupActionButtonStyle:(UIButton*)button;
-(void)setupNavigationBar;
-(void)dismissPopover;
-(void)dismissPopoverAnimated:(BOOL)animated;
-(ProviderPhotoSource*)createFilteredSource:(ProviderPhoto*)existingPhoto newPhoto:(ProviderPhoto**)newPhoto;
-(void)setupIPhoneSecondBarForceRecreate:(BOOL)forceRecreate;
-(void)rotateIPhoneSecondBar;
-(void)showNavigationBar;
-(IBAction)deleteSelected:(id)sender;
-(IBAction)modifySelected:(id)sender;
-(IBAction)toggleSelection:(id)sender;
-(void)showSendPopover:(CommonPopoverController*)content button:(id)sender action:(CommonPopoverBlock)actionBlock;
-(void)extraHeaderUpdatedAnimated:(BOOL)animated;
-(void)pushNewSource:(ProviderPhotoSource*)newPhotos indexPath:(NSIndexPath*)indexPath;
-(void)pushNewSource:(ProviderPhotoSource*)newPhotos indexPath:(NSIndexPath*)indexPath viewController:(FSBaseViewController*)anotherViewController;
-(void)addEmptyLabel:(NSString*)message;
-(FSBaseViewController*)cloneWithViewSituation:(ViewSituation)newViewSituation;
-(void)adjustTabBar:(UITabBar*)tabBar;
-(void)resetStatusAndNavigationBars;
-(void)showOnMacWindow;
-(void)macWindowClose;
-(void)macWindowDidClose:(BOOL)closePressed;
-(void)viewDidLoadMac;
-(void)viewResizedMac;
-(void)hideMainNavigationBar:(BOOL)hidden;
@property(nonatomic, readonly) BOOL shouldHideBackButton;
@property(nonatomic, readonly) BOOL supportsRefresh;
@property(nonatomic, readonly) BOOL shouldHideNavigationBarOnScroll;
@property(nonatomic, readonly) BOOL isPopover;
@property(nonatomic, readonly) CGFloat statusBarHeight;
@property(nonatomic, strong) NSString *featuredId;
@property(nonatomic, strong) ProviderPhotoSource *photos;
@property(nonatomic, strong) FSBaseViewController *parentSwapView;
@property(nonatomic, strong) FSPopoverController* menuPopover;
@property(nonatomic, strong) DragRefreshView* refreshHeaderView;
@property(nonatomic, strong) IPWaitLabel *loadingLabel;
@property(nonatomic, strong) TipLabel* toolbarPressedLabel;
@property(nonatomic, strong) UIView *iPhoneSecondBar;
@property(nonatomic, strong) UIView *emptyLabel;
@property(nonatomic, strong) id<FSExtraPhotoSetHeader> extraPhotoHeader;
@property(nonatomic, strong) id<FSExtraPhotoSetComplexHeader> extraComplexHeader;
@property(nonatomic, assign) BOOL userInitiatedRefresh;
@property(nonatomic, readonly) FSWindowController *windowController;
@property(nonatomic, readonly) FSToolbarButton currentToolbarButtons;
@end

@interface FSBaseViewController(FSPlatform)
-(UITableViewCell*)loadCellFromBundle:(NSString*)name;
-(PhotoCell*)loadPhotoCellFromBundle:(NSString*)name;
@end