#import "objd.h"

@class CNEnum;

@interface CNEnum : NSObject
@property (nonatomic, readonly) NSUInteger ordinal;
@property (nonatomic, readonly) NSString* name;

+ (id)enumWithOrdinal:(NSUInteger)ordinal name:(NSString*)name;
- (id)initWithOrdinal:(NSUInteger)ordinal name:(NSString*)name;
- (NSString*)description;
- (BOOL)isEqual:(id)other;
- (NSUInteger)hash;

- (NSInteger)compareTo:(CNEnum *)to;
@end


