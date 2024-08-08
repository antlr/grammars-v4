#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

// Test lightweight generics.
@interface GenericTabBarItemContainerDataSource<State, VC : UIViewController *> : GenericDeckContainerDataSource <State, VC>

- (instancetype)initWithBuilder:(VC (^)(State))builder
    preserveStateBlock:(nullable State (^)(VC))preserveStateBlock
    purgeBehavior:(GenericDeckContainerDataSourcePurgeBehavior)purgeBehavior
    purgeDelay:(NSInteger)purgeDelay
    tabBarItemContainer:(id<TabBarItemContainer>)tabBarItemContainer;

@end

// Test covariant
@interface AccessOrderedDictionary<__covariant KeyType, __covariant ObjectType> : OrderedDictionary <NSCoding>

- (instancetype)initWithMaxSize:(NSInteger)maxSize;

- (ObjectType)objectForKey:(KeyType)key updateOrder:(BOOL)updateOrder;

@end

NS_ASSUME_NONNULL_END
