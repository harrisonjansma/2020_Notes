import tensorflow as     tf
import math
import numpy             as np
import matplotlib.pyplot as plt
from tensorflow.keras.layers import *

DATA_NUM_CLASSES        = 10
DATA_CHANNELS           = 3
DATA_ROWS               = 32
DATA_COLS               = 32
DATA_CROP_ROWS          = 28
DATA_CROP_COLS          = 28
DATA_MEAN               = np.array([[[125.30691805, 122.95039414, 113.86538318]]]) # CIFAR10
DATA_STD_DEV            = np.array([[[ 62.99321928,  62.08870764,  66.70489964]]]) # CIFAR10

# model
MODEL_LEVEL_0_BLOCKS    = 4
MODEL_LEVEL_1_BLOCKS    = 4
MODEL_LEVEL_2_BLOCKS    = 5
BLOCK_REPEATS = [MODEL_LEVEL_0_BLOCKS,MODEL_LEVEL_1_BLOCKS,MODEL_LEVEL_2_BLOCKS]

# training
TRAINING_BATCH_SIZE      = 64
TRAINING_SHUFFLE_BUFFER  = 5000
TRAINING_BN_MOMENTUM     = 0.9
TRAINING_BN_EPSILON      = 0.001

TRAINING_LR_MAX          = 0.001
TRAINING_LR_INIT_SCALE   = 0.01
TRAINING_LR_INIT_EPOCHS  = 5
TRAINING_LR_FINAL_SCALE  = 0.01
TRAINING_LR_FINAL_EPOCHS = 25

# training (derived)
TRAINING_NUM_EPOCHS = TRAINING_LR_INIT_EPOCHS + TRAINING_LR_FINAL_EPOCHS
TRAINING_LR_INIT    = TRAINING_LR_MAX*TRAINING_LR_INIT_SCALE
TRAINING_LR_FINAL   = TRAINING_LR_MAX*TRAINING_LR_FINAL_SCALE

# saving
SAVE_MODEL_PATH = 'F://Models/Model_Design/'

######################################################################
# MODEL FUNCTIONS
######################################################################

conv_params = {"padding":'same',
              "use_bias":False,
              "activation":None}

bn_params = {"axis":-1,
             "momentum":TRAINING_BN_MOMENTUM, 
             "epsilon":TRAINING_BN_EPSILON, 
             "center":True, 
             "scale":True}

def conv_block(inputs, filters, kernel_size=(3,3), strides=(1,1)):
    """Generic Conv -> BN -> ReLU abstraction"""
    x = Conv2D(filters, kernel_size, strides=strides, **conv_params)(inputs)
    x = BatchNormalization(**bn_params)(x)
    x = ReLU()(x)
    return x  

def VGG_Like_CNN(tail_function, block_function, head_function, input_shape=None, num_levels=None, block_repeats=None, num_downsamples=None, start_dims=32):
    """
    INPUTS:
    tail_function: function(in_tensor, dims, **kwargs) -> out tensor
    block_function: function(in_tensor, dims, downsampling=Bool, **kwargs) -> out tensor
    head_function: function(in_tensor, dims=None, **kwargs) -> out tensor
    input_shape: tuple- input shape of the network ex. (15,) or (3,32,32)
    num_levels: int - number of levels in the VGG-like architecture
    block_repeats: list - number of block repeats for each level of VGG-like architecture
    num_downsamples: int - optional max number of downsamples to stop at
    start_dims: int - number of filters to be given to the head block. will double each level
    """
    model_input = Input(shape=input_shape)
    dims = int(start_dims)
    
    #TAIL
    x = tail_function(model_input, dims)
    
    #BODY
    for level in range(num_levels):
        for block in range(block_repeats[level]):
            x = block_function(x, dims)
            
        if num_downsamples is not None:
            if level+1>num_downsamples: #reached max num_downsamples
                continue
                
        dims = int(dims*2)
        x = block_function(x, dims, downsample=True)
        
    #HEAD
    model_output = head_function(x, dims=dims)
    return tf.keras.Model(inputs=model_input, outputs=model_output)




def get_num_params(MODEL):
    """https://stackoverflow.com/questions/38160940/how-to-count-total-number-of-trainable-parameters-in-a-tensorflow-model"""
    total_params=1
    for variable in MODEL.trainable_variables:
        variable_params = 1
        for dim in variable.shape.as_list():
            variable_params*=dim
        total_params+=variable_params
    return total_params












######################################################################
# TRAINING FUNCTIONS FOR THE CIFAR DATASET
######################################################################
def pre_processing_train(example):
    image = example["image"]
    label = example["label"]
    image = tf.math.divide(tf.math.subtract(tf.dtypes.cast(image, tf.float32), DATA_MEAN), DATA_STD_DEV)
    image = tf.image.random_flip_left_right(image)
    image = tf.image.random_crop(image, size=[DATA_CROP_ROWS, DATA_CROP_COLS, 3])
    label = tf.dtypes.cast(label, tf.int32)
    return image, label

def pre_processing_test(example):
    image = example["image"]
    label = example["label"]
    image = tf.math.divide(tf.math.subtract(tf.dtypes.cast(image, tf.float32), DATA_MEAN), DATA_STD_DEV)
    image = tf.image.crop_to_bounding_box(image, (DATA_ROWS - DATA_CROP_ROWS) // 2, (DATA_COLS - DATA_CROP_COLS) // 2, DATA_CROP_ROWS, DATA_CROP_COLS)
    label = tf.dtypes.cast(label, tf.int32)
    return image, label

# learning rate schedule
def lr_schedule(epoch):
    if epoch < TRAINING_LR_INIT_EPOCHS:
        lr = (TRAINING_LR_MAX - TRAINING_LR_INIT)*(float(epoch)/TRAINING_LR_INIT_EPOCHS) + TRAINING_LR_INIT
    else:
        lr = (TRAINING_LR_MAX - TRAINING_LR_FINAL)*max(0.0, math.cos(((float(epoch) - TRAINING_LR_INIT_EPOCHS)/(TRAINING_LR_FINAL_EPOCHS - 1.0))*(math.pi/2.0))) + TRAINING_LR_FINAL
    return lr

def plot_training_curves(history):
    # training and validation data accuracy
    acc     = history.history['accuracy']
    val_acc = history.history['val_accuracy']

    # training and validation data loss
    loss     = history.history['loss']
    val_loss = history.history['val_loss']

    # plot accuracy
    plt.figure(figsize=(8, 8))
    plt.subplot(2, 1, 1)
    plt.plot(acc, label='Training Accuracy')
    plt.plot(val_acc, label='Validation Accuracy')
    plt.legend(loc='lower right')
    plt.ylabel('Accuracy')
    plt.ylim([min(plt.ylim()), 1])
    plt.title('Training and Validation Accuracy')

    # plot loss
    plt.subplot(2, 1, 2)
    plt.plot(loss, label='Training Loss')
    plt.plot(val_loss, label='Validation Loss')
    plt.legend(loc='upper right')
    plt.ylabel('Cross Entropy')
    plt.ylim([0, 2.0])
    plt.title('Training and Validation Loss')
    plt.xlabel('epoch')
    plt.show()


def train(MODEL,train, test, model_name, logs=False):
    """
    Inputs:
    MODEL: tf.keras.Model - used for training
    train, tests: tf.Datasets
    model_name: string - name of the trained model
    logs: Bool - whether to print logs for each training epoch
    
    Trains MODEL for TRAINING_NUM_EPOCHS epochs, saving best model at 
    each epoch. Plots the training curve and evaluates the final model on
    the validation dataset.
    
    """
    print("######################################################")
    print(model_name)
    print("######################################################")
    
    callbacks = [tf.keras.callbacks.LearningRateScheduler(lr_schedule),
                       tf.keras.callbacks.ModelCheckpoint(filepath=str(SAVE_MODEL_PATH)+model_name+'/', 
                                                    save_best_only=True, 
                                                    period=10,
                                                    monitor='val_loss', 
                                                        verbose=0),
                 tf.keras.callbacks.CSVLogger(str(SAVE_MODEL_PATH)+model_name+'/train_log.csv'),
                tf.keras.callbacks.EarlyStopping(patience=4)]
    # training
    initial_epoch_num = 0
    print("Training model {}...".format(model_name))
    history  = MODEL.fit(x=train, 
                          epochs=TRAINING_NUM_EPOCHS, 
                          verbose=logs, 
                          callbacks=callbacks, 
                          validation_data=test,                               
                          initial_epoch=initial_epoch_num)

    print("Training complete.")
    return history

def benchmark(MODEL,test, history, model_name):
    # plot accuracy and loss curves
    plot_training_curves(history)
    # test
    test_loss, test_accuracy = MODEL.evaluate(x=test)
    print('Test loss:     ', test_loss)
    print('Test accuracy: ', test_accuracy)