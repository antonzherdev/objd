#import "objdcore.h"
#import "ODObject.h"
@class ODClassType;

@class CNSeed;

@interface CNSeed : NSObject {
@private
    unsigned int _id;
    unsigned int _circleSize;
}
@property (nonatomic, readonly) unsigned int id;
@property (nonatomic, readonly) unsigned int circleSize;
@property(nonatomic) unsigned int position;

+ (instancetype)seedWithId:(unsigned int)id circleSize:(unsigned int)circleSize;
- (instancetype)initWithId:(unsigned int)id circleSize:(unsigned int)circleSize;
- (ODClassType*)type;
- (int)nextInt;
- (int)nextIntMin:(int)min max:(int)max;
+ (CNSeed*)applyId:(unsigned int)id;
+ (CNSeed*)applyCircleSize:(unsigned int)circleSize;
+ (CNSeed*)apply;
+ (ODClassType*)type;
@end


