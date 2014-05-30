#import "objd.h"
#import "CNSeed.h"
#import "CNType.h"
#define SEED_RAND(f) \
    int r = _rands[_position] f;\
    _position++;\
    if(_position > _circleSize) _position = 0;\
    return r;

@implementation CNSeed {
    int * _rands;
    unsigned int _position;
}
static CNClassType * _CNSeed_type;
@synthesize id = _id;
@synthesize circleSize = _circleSize;
@synthesize position = _position;

+ (instancetype)seedWithId:(unsigned int)id circleSize:(unsigned int)circleSize {
    return [[CNSeed alloc] initWithId:id circleSize:circleSize];
}

- (instancetype)initWithId:(unsigned int)id circleSize:(unsigned int)circleSize {
    self = [super init];
    if(self) {
        _id = id;
        _circleSize = circleSize;
        _rands = malloc(sizeof(int)*circleSize);
        srand((unsigned int) (id == 0 ? time(NULL) : id));
        int* p = _rands;
        for(unsigned int i = 0; i < circleSize; i++) {
            *p = rand();
            p++;
        }
    }
    
    return self;
}

- (void)dealloc {
    free(_rands);
}


+ (void)initialize {
    [super initialize];
    if(self == [CNSeed class]) _CNSeed_type = [CNClassType classTypeWithCls:[CNSeed class]];
}

- (int)nextInt {
    SEED_RAND( + 0)
}

- (int)nextIntMin:(int)min max:(int)max {
    if(min == max) {
        _position++;
        if(_position > _circleSize) _position = 0;
        return min;
    }
    SEED_RAND(% (max - min) + min)
}


+ (CNSeed*)applyId:(unsigned int)id {
    return [CNSeed seedWithId:id circleSize:1000];
}

+ (CNSeed*)applyCircleSize:(unsigned int)circleSize {
    return [CNSeed seedWithId:0 circleSize:circleSize];
}

+ (CNSeed*)apply {
    return [CNSeed seedWithId:0 circleSize:1000];
}

- (CNClassType *)type {
    return [CNSeed type];
}

+ (CNClassType *)type {
    return _CNSeed_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"id=%u", self.id];
    [description appendFormat:@", circleSize=%u", self.circleSize];
    [description appendString:@">"];
    return description;
}

@end


