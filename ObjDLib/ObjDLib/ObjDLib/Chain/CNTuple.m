#import "objd.h"
#import "CNTuple.h"

#import "ODType.h"
@implementation CNTuple{
    id _a;
    id _b;
}
static ODClassType* _CNTuple_type;
@synthesize a = _a;
@synthesize b = _b;

+ (id)tupleWithA:(id)a b:(id)b {
    return [[CNTuple alloc] initWithA:a b:b];
}

- (id)initWithA:(id)a b:(id)b {
    self = [super init];
    if(self) {
        _a = a;
        _b = b;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNTuple_type = [ODClassType classTypeWithCls:[CNTuple class]];
}

- (NSInteger)compareTo:(CNTuple*)to {
    NSInteger r = [to.a compareTo:_a];
    if(r == 0) return -[to.b compareTo:_b];
    else return -r;
}

- (NSString*)description {
    return [NSString stringWithFormat:@"(%@, %@)", _a, _b];
}

+ (id)unapplyTuple:(CNTuple*)tuple {
    return [CNOption applyValue:tuple];
}

- (ODClassType*)type {
    return [CNTuple type];
}

+ (ODClassType*)type {
    return _CNTuple_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNTuple* o = ((CNTuple*)(other));
    return [self.a isEqual:o.a] && [self.b isEqual:o.b];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.a hash];
    hash = hash * 31 + [self.b hash];
    return hash;
}

@end


@implementation CNTuple3{
    id _a;
    id _b;
    id _c;
}
static ODClassType* _CNTuple3_type;
@synthesize a = _a;
@synthesize b = _b;
@synthesize c = _c;

+ (id)tuple3WithA:(id)a b:(id)b c:(id)c {
    return [[CNTuple3 alloc] initWithA:a b:b c:c];
}

- (id)initWithA:(id)a b:(id)b c:(id)c {
    self = [super init];
    if(self) {
        _a = a;
        _b = b;
        _c = c;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNTuple3_type = [ODClassType classTypeWithCls:[CNTuple3 class]];
}

- (NSInteger)compareTo:(CNTuple3*)to {
    NSInteger r = [to.a compareTo:_a];
    if(r == 0) {
        r = [to.b compareTo:_b];
        if(r == 0) return -[to.c compareTo:_c];
        else return -r;
    } else {
        return -r;
    }
}

- (NSString*)description {
    return [NSString stringWithFormat:@"(%@, %@, %@)", _a, _b, _c];
}

+ (id)unapplyTuple:(CNTuple3*)tuple {
    return [CNOption applyValue:tuple];
}

- (ODClassType*)type {
    return [CNTuple3 type];
}

+ (ODClassType*)type {
    return _CNTuple3_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNTuple3* o = ((CNTuple3*)(other));
    return [self.a isEqual:o.a] && [self.b isEqual:o.b] && [self.c isEqual:o.c];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.a hash];
    hash = hash * 31 + [self.b hash];
    hash = hash * 31 + [self.c hash];
    return hash;
}

@end


@implementation CNTuple4{
    id _a;
    id _b;
    id _c;
    id _d;
}
static ODClassType* _CNTuple4_type;
@synthesize a = _a;
@synthesize b = _b;
@synthesize c = _c;
@synthesize d = _d;

+ (id)tuple4WithA:(id)a b:(id)b c:(id)c d:(id)d {
    return [[CNTuple4 alloc] initWithA:a b:b c:c d:d];
}

- (id)initWithA:(id)a b:(id)b c:(id)c d:(id)d {
    self = [super init];
    if(self) {
        _a = a;
        _b = b;
        _c = c;
        _d = d;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNTuple4_type = [ODClassType classTypeWithCls:[CNTuple4 class]];
}

- (NSInteger)compareTo:(CNTuple4*)to {
    NSInteger r = [to.a compareTo:_a];
    if(r == 0) {
        r = [to.b compareTo:_b];
        if(r == 0) {
            r = [to.c compareTo:_c];
            if(r == 0) return -[to.d compareTo:_d];
            else return -r;
        } else {
            return -r;
        }
    } else {
        return -r;
    }
}

- (NSString*)description {
    return [NSString stringWithFormat:@"(%@, %@, %@, %@)", _a, _b, _c, _d];
}

+ (id)unapplyTuple:(CNTuple4*)tuple {
    return [CNOption applyValue:tuple];
}

- (ODClassType*)type {
    return [CNTuple4 type];
}

+ (ODClassType*)type {
    return _CNTuple4_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNTuple4* o = ((CNTuple4*)(other));
    return [self.a isEqual:o.a] && [self.b isEqual:o.b] && [self.c isEqual:o.c] && [self.d isEqual:o.d];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.a hash];
    hash = hash * 31 + [self.b hash];
    hash = hash * 31 + [self.c hash];
    hash = hash * 31 + [self.d hash];
    return hash;
}

@end


