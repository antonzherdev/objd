#import "objdcore.h"
#import "CNObject.h"
@class CNClassType;

@class CNEnum;

@interface CNEnum : NSObject<CNComparable> {
@protected
    NSUInteger _ordinal;
    NSString* _name;
}
@property (nonatomic, readonly) NSUInteger ordinal;
@property (nonatomic, readonly) NSString* name;

+ (instancetype)enumWithOrdinal:(NSUInteger)ordinal name:(NSString*)name;
- (instancetype)initWithOrdinal:(NSUInteger)ordinal name:(NSString*)name;
- (CNClassType*)type;
- (NSString*)description;
- (NSUInteger)hash;
+ (NSArray*)values;
- (NSInteger)compareTo:(CNEnum*)to;
+ (CNClassType*)type;
@end


